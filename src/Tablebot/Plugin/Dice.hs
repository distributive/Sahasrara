-- |
-- Module      : Tablebot.Plugin.Dice
-- Description : Lex, parse, and evaluate dice and other expressions using this plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the neccessary parsers and stucture to get the AST for an
-- expression that contains dice, as well as evaluate that expression.
module Tablebot.Plugin.Dice (evalExpr, Expr, PrettyShow (..), supportedFunctionsList, defaultRoll) where

import Control.Monad (when)
import Control.Monad.Exception (MonadException)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor (($>), (<&>))
import Data.List (genericDrop, genericReplicate, genericTake, intercalate, sortBy)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, tail, (<|))
import Data.Map as M (Map, findWithDefault, fromList, keys, map, member)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (pack, unpack)
import Data.Tuple (swap)
import System.Random (Random (randomRIO))
import Tablebot.Plugin.Exception (BotException (EvaluationException), catchBot, throwBot)
import Tablebot.Plugin.Parser (posInteger, skipSpace, skipSpace1, word)
import Tablebot.Plugin.Random (chooseOne)
import Tablebot.Plugin.SmartCommand (CanParse (..), FromString (fromString))
import Tablebot.Plugin.Types (Parser)
import Text.Megaparsec (MonadParsec (try), many, optional, (<?>), (<|>))
import Text.Megaparsec.Char (char, string)

-- TODO: update the parsing stuff below so people can actually make sense of the stuff down below
{- What is the parse tree from lowest precedence to highest?

addition/subtraction [expr] {term [+-] term}
multiplication/integer division [term] {func [*/] func}
function application [func] {`func name` " "+ neg}
negation [neg] {"-" exp}
exponentiation [exp] {base "^" base}
brackets, integer, dice [base] {"(" expr ")"} {[0123456789]+} {dieops}
--- the dice resolution barrier
(keep/drop) (lowest/highest), reroll once, reroll [dieops] {[kd][lh] [0123456789]+} {"rro" (<|>|=|<=|>=) [0123456789]+} {"rr" (<|>|=|<=|>=) [0123456789]+}
multiple dice [mdie] {[0123456789]+ bdie}
base die [bdie] {"d" base} {"d{" [0123456789]+ ("," [0123456789]+)* "}"}
-}

-- | The maximum depth that should be permitted. Used to limit number of dice and rerolls.
maximumRNG :: Integer
maximumRNG = 150

-- | Check whether the RNG count has been exceeded by the interger given.
checkRNGCount :: Integer -> IO ()
checkRNGCount i =
  when (i > maximumRNG) $ throwBot $ EvaluationException ("exceeded maximum rng count (" ++ show maximumRNG ++ ")") []

-- | The default expression to evaluate if no expression is given.
defaultRoll :: Expr
defaultRoll = NoExpr $ NoTerm $ Func "id" $ NoNeg $ NoExpo $ DiceBase $ Dice (NBase (Value 1)) (Die (Value 20)) Nothing

-- | The limit to how big a factorial value is permitted. Notably, the factorial function doesn't operate above this limit.
factorialLimit :: Integer
factorialLimit = 50

-- TODO: full check over of bounds. make this thing AIR TIGHT.

-- | The type of the top level expression. Represents one of addition, subtraction, or a
-- single term.
data Expr = Add Term Expr | Sub Term Expr | NoExpr Term
  deriving (Show, Eq)

-- | The type representing multiplication, division, or a single function application.
data Term = Multi Func Term | Div Func Term | NoTerm Func
  deriving (Show, Eq)

-- | The type representing a single function application on a negated item.
data Func = Func String Negation
  deriving (Show, Eq)

-- | The type representing a possibly negated value.
data Negation = Neg Expo | NoNeg Expo
  deriving (Show, Eq)

-- | The type representing a value with exponentials.
data Expo = Expo Base Expo | NoExpo Base
  deriving (Show, Eq)

-- | The type representing an integer value or an expression in brackets.
data NumBase = Paren Expr | Value Integer
  deriving (Show, Eq)

-- | The type representing a numeric base value value or a dice value.
data Base = NBase NumBase | DiceBase Dice
  deriving (Show, Eq)

fromIntegerToExpr :: Integer -> Expr
fromIntegerToExpr = NoExpr . NoTerm . Func "id" . NoNeg . NoExpo . NBase . Value

-- Dice Operations after this point

-- TODO: create a lazy and a repeated eval die so that sometimes dice quantities are eval'd
-- each time and sometimes are eval'd once and then that value is used

-- | The type representing a simple N sided die or a custom die.
data Die = Die NumBase | CustomDie [Expr] | LazyDie Die deriving (Show, Eq)

-- | The type representing a number of dice equal to the `Base` value, and possibly some
-- die options.
data Dice = Dice Base Die (Maybe DieOpRecur)
  deriving (Show, Eq)

-- | The type representing one or more die options.
data DieOpRecur = DieOpRecur DieOpOption (Maybe DieOpRecur)
  deriving (Show, Eq)

data AdvancedOrdering = Not AdvancedOrdering | OrderingId Ordering | And [AdvancedOrdering] | Or [AdvancedOrdering]
  deriving (Show, Eq, Ord)

applyCompare :: Ord a => AdvancedOrdering -> a -> a -> Bool
applyCompare (OrderingId o) a b = o == compare a b
applyCompare (And os) a b = all (\o -> applyCompare o a b) os
applyCompare (Or os) a b = any (\o -> applyCompare o a b) os
applyCompare (Not o) a b = not (applyCompare o a b)

advancedOrderingMapping :: (FromString a, Ord a) => (Map a AdvancedOrdering, Map AdvancedOrdering a)
advancedOrderingMapping = (M.fromList lst, M.fromList $ swap <$> lst)
  where
    lst =
      fmap
        (first fromString)
        [ ("!=", Not (OrderingId EQ)),
          ("<=", Or [OrderingId EQ, OrderingId LT]),
          (">=", Or [OrderingId EQ, OrderingId GT]),
          ("<", OrderingId LT),
          ("=", OrderingId EQ),
          (">", OrderingId GT)
        ]

-- | The type representing a die option.
data DieOpOption
  = Reroll {rerollOnce :: Bool, condition :: AdvancedOrdering, limit :: NumBase}
  | DieOpOptionKD KeepDrop LowHighWhere
  | DieOpOptionLazy DieOpOption
  deriving (Show, Eq)

-- | A type used to designate how the keep/drop option should work
data LowHighWhere = Low NumBase | High NumBase | Where AdvancedOrdering NumBase deriving (Show, Eq)

-- | Utility function to get the integer determining how many values to get given a
-- `LowHighWhere`. If the given value is `Low` or `High`, then Just the NumBase contained
-- is returned. Else, Nothing is returned.
getValueLowHigh :: LowHighWhere -> Maybe NumBase
getValueLowHigh (Low i) = Just i
getValueLowHigh (High i) = Just i
getValueLowHigh (Where _ _) = Nothing

-- | Returns whether the given `LowHighWhere` is `Low` or not.
isLow :: LowHighWhere -> Bool
isLow (Low _) = True
isLow _ = False

-- | Utility value for whether to keep or drop values.
data KeepDrop = Keep | Drop deriving (Show, Eq)

-- Mappings for what functions are supported

-- | Mapping from function names to the functions themselves.
supportedFunctions :: Map String (Integer -> Integer)
supportedFunctions =
  M.fromList
    [ ("abs", abs),
      ("id", id),
      ("fact", fact),
      ("neg", negate)
    ]
  where
    fact n
      | n < 0 = 0
      | n == 0 = 1
      | n > factorialLimit = fact factorialLimit
      | otherwise = n * fact (n - 1)

supportedFunctionsList :: [String]
supportedFunctionsList = M.keys supportedFunctions

-- | Functions that looks up the given function name in the map, and will either throw an
-- error or return the function (wrapped inside the given monad)
getFunc :: MonadException m => String -> m (Integer -> Integer)
getFunc s = M.findWithDefault (throwBot $ EvaluationException ("could not find function `" ++ s ++ "`") []) s (M.map return supportedFunctions)

--- Evaluating an expression. Uses IO because dice are random

-- | Given an expression, evaluate it, getting the pretty printed string and the value of
-- the result
evalExpr :: Expr -> IO (Integer, String, Integer)
evalExpr = evalShow 0

-- | Utility function to display dice.
--
-- The tuple of integers denotes what the critvalues of this dice value are. The `a`
-- denotes the value that is being printed, and needs to have `PrettyShow` defined for it.
-- Finally, the list of tuples denotes all the values that the `a` value has gone through.
-- If the `Maybe Bool` value is `Nothing`, the number is displayed as normal. If the value
-- is `Just False`, the value has been rerolled over, and is displayed crossed out. If the
-- value is `Just True`, the value has been dropped, and the number is crossed out and
-- underlined.
dieShow :: (PrettyShow a, MonadException m) => Maybe (Integer, Integer) -> a -> [(Integer, Maybe Bool)] -> m String
dieShow _ a [] = throwBot $ EvaluationException "tried to show empty set of results" [prettyShow a]
dieShow lchc d ls = return $ prettyShow d ++ " [" ++ foldr1 (\n rst -> n ++ ", " ++ rst) adjustList ++ "]"
  where
    toCrit =
      if isNothing lchc
        then show
        else toCrit'
    (lc, hc) = fromMaybe (0, 0) lchc
    toCrit' i
      | i == lc || i == hc = "**" ++ show i ++ "**"
      | otherwise = show i
    toCrossedOut (i, Just False) = "~~" ++ toCrit i ++ "~~"
    toCrossedOut (i, Just True) = "~~__" ++ toCrit i ++ "__~~"
    toCrossedOut (i, _) = toCrit i
    adjustList = fmap toCrossedOut ls

-- | This type class gives a function which evaluates the value to an integer and a
-- string.
class IOEval a where
  -- | Evaluate the given item to an integer, a string representation of the value, and
  -- the number of RNG calls it took. If the `a` value is a dice value, the values of the
  -- dice should be displayed. The integer given initially is the current RNG count of the
  -- expression. This function adds the current location to the exception callstack.
  evalShow :: PrettyShow a => Integer -> a -> IO (Integer, String, Integer)
  evalShow rngCount a = catchBot (evalShow' rngCount a) handleException
    where
      handleException (EvaluationException msg' locs) = throwBot (EvaluationException msg' (addIfNotIn locs))
      handleException e = throwBot e
      pa = prettyShow a
      addIfNotIn locs = if null locs || pa /= Prelude.head locs then pa : locs else locs

  evalShow' :: Integer -> a -> IO (Integer, String, Integer)

instance IOEval Base where
  evalShow' rngCount (NBase nb) = evalShow rngCount nb
  evalShow' rngCount (DiceBase dice) = evalShow rngCount dice

instance IOEval Die where
  evalShow' rngCount ld@(LazyDie d) = do
    (i, _, rngCount') <- evalShow rngCount d
    ds <- dieShow Nothing ld [(i, Nothing)]
    return (i, ds, rngCount')
  evalShow' rngCount d@(CustomDie is) = do
    i <- chooseOne is
    (i', _, rngCount') <- evalShow rngCount i
    ds <- dieShow Nothing d [(i', Nothing)]
    checkRNGCount (rngCount' + 1)
    return (i', ds, rngCount' + 1)
  evalShow' rngCount d@(Die b) = do
    (bound, _, rngCount') <- evalShow rngCount b
    if bound < 1
      then throwBot $ EvaluationException ("Cannot roll a < 1 sided die (`" ++ prettyShow b ++ "`)") []
      else do
        i <- randomRIO (1, bound)
        ds <- dieShow Nothing d [(i, Nothing)]
        checkRNGCount (rngCount' + 1)
        return (i, ds, rngCount' + 1)

instance IOEval Dice where
  evalShow' rngCount dop = do
    (lst, mnmx, rngCount') <- evalDieOp rngCount dop
    let vs = fromEvalDieOpList lst
    s <- dieShow mnmx dop vs
    return (sum (fst <$> filter (isNothing . snd) vs), s, rngCount')

-- | Utility function to transform the output list type of other utility functions into
-- one that `dieShow` recognises
fromEvalDieOpList :: [(NonEmpty Integer, Bool)] -> [(Integer, Maybe Bool)]
fromEvalDieOpList = foldr foldF []
  where
    foldF (is, b) lst = let is' = (,Just False) <$> NE.tail is in (reverse ((NE.head is, if b then Nothing else Just True) : is') ++ lst)

-- | Helper function that takes a set of Dice and returns a tuple of three items. The
-- second item is the maximum and minimum value of the base die.
-- The first item is a list representing each die - a tuple with a history of the die
-- being rolled, and whether the die has been dropped or not. The first item of each die
-- record is the current value of the die. The last item of the tuple is how many calls to
-- RNG there have been.
--
-- The function itself checks to make sure the number of dice being rolled is less than
-- the maximum recursion and is non-negative.
evalDieOp :: Integer -> Dice -> IO ([(NonEmpty Integer, Bool)], Maybe (Integer, Integer), Integer)
evalDieOp rngCount (Dice b ds dopo) = do
  (nbDice, _, rngCountB) <- evalShow rngCount b
  if nbDice > maximumRNG
    then throwBot (EvaluationException ("tried to roll more than " ++ show maximumRNG ++ " dice: " ++ show nbDice) [prettyShow b])
    else do
      if nbDice < 0
        then throwBot (EvaluationException ("tried to give a negative value to the number of dice: " ++ show nbDice) [prettyShow b])
        else do
          (ds', rngCountCondense, crits) <- condenseDie rngCountB ds
          (rolls, rngCountRolls) <- foldr foldF (return ([], rngCountCondense)) (genericReplicate nbDice ds')
          let vs = fmap (\i -> (i :| [], True)) rolls
          (rs, rngCountRs) <- evalDieOp' rngCountRolls dopo ds' vs
          return (sortBy sortByOption rs, crits, rngCountRs)
  where
    condenseDie rngCount' (Die dBase) = do
      (i, _, rngCount'') <- evalShow rngCount' dBase
      return (Die (Value i), rngCount'', Just (1, i))
    condenseDie rngCount' (CustomDie is) = do
      (is', rngCount'') <- foldr foldF (return ([], rngCount')) is
      return (CustomDie (fromIntegerToExpr <$> is'), rngCount'', Nothing)
    condenseDie rngCount' (LazyDie d) = return (d, rngCount', Nothing)
    foldF die sumrngcount = do
      (diceSoFar, rngCountTotal) <- sumrngcount
      (i, _, rngCountTemp) <- evalShow rngCountTotal die
      return (i : diceSoFar, rngCountTemp)
    sortByOption (e :| es, _) (f :| fs, _)
      | e == f = compare (length fs) (length es)
      | otherwise = compare e f

-- | Utility function that processes a `Maybe DieOpRecur`, when given a range for dice,
-- and dice that have already been processed.
evalDieOp' :: Integer -> Maybe DieOpRecur -> Die -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOp' rngCount Nothing _ is = return (is, rngCount)
evalDieOp' rngCount (Just (DieOpRecur doo mdor)) die is = do
  (doo', rngCount') <- processDOO rngCount doo
  (is', rngCount'') <- evalDieOp'' rngCount' doo' die is
  evalDieOp' rngCount'' mdor die is'
  where
    processLHW rngCount' (Low i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (Low (Value i'), rngCount'')
    processLHW rngCount' (High i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (High (Value i'), rngCount'')
    processLHW rngCount' (Where o i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (Where o (Value i'), rngCount'')
    processDOO rngCount' (DieOpOptionKD kd lhw) = do
      (lhw', rngCount'') <- processLHW rngCount' lhw
      return (DieOpOptionKD kd lhw', rngCount'')
    processDOO rngCount' (Reroll once o i) = do
      (i', _, rngCount'') <- evalShow rngCount' i
      return (Reroll once o (Value i'), rngCount'')
    processDOO rngCount' (DieOpOptionLazy doo') = return (doo', rngCount')

-- | Utility function that processes a `DieOpOption`, when given a range for dice,
-- and dice that have already been processed.
evalDieOp'' :: Integer -> DieOpOption -> Die -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOp'' rngCount (DieOpOptionLazy doo) die is = evalDieOp'' rngCount doo die is
evalDieOp'' rngCount (DieOpOptionKD kd lhw) _ is = evalDieOpHelpKD rngCount kd lhw is
evalDieOp'' rngCount (Reroll once o i) die is = foldr rerollF (return ([], rngCount)) is
  where
    rerollF g@(i', b) isRngCount' = do
      (is', rngCount') <- isRngCount'
      (iEval, _, rngCount'') <- evalShow rngCount' i
      if b && applyCompare o (NE.head i') iEval
        then do
          (v, _, rngCount''') <- evalShow rngCount'' die
          let ret = (v <| i', b)
          if once
            then return (ret : is', rngCount''')
            else rerollF ret (return (is', rngCount'''))
        else return (g : is', rngCount'')

-- | Given a list of dice values, separate them into kept values and dropped values
-- respectively.
separateKeptDropped :: [(NonEmpty Integer, Bool)] -> ([(NonEmpty Integer, Bool)], [(NonEmpty Integer, Bool)])
separateKeptDropped = foldr f ([], [])
  where
    f a@(_, True) (kept, dropped) = (a : kept, dropped)
    f a@(_, False) (kept, dropped) = (kept, a : dropped)

-- | Utility function to set all the values in the given list to be dropped.
setToDropped :: [(NonEmpty Integer, Bool)] -> [(NonEmpty Integer, Bool)]
setToDropped = fmap (\(is, _) -> (is, False))

-- | Helper function that executes the keep/drop commands on dice.
evalDieOpHelpKD :: Integer -> KeepDrop -> LowHighWhere -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOpHelpKD rngCount kd (Where cmp i) is = foldr foldF (return ([], rngCount)) is
  where
    isKeep = if kd == Keep then id else not
    foldF (iis, b) sumrngcount = do
      (diceSoFar, rngCountTotal) <- sumrngcount
      (i', _, rngCountTemp) <- evalShow rngCountTotal i
      return ((iis, b && isKeep (applyCompare cmp (NE.head iis) i')) : diceSoFar, rngCountTemp)
evalDieOpHelpKD rngCount kd lh is = do
  (i', _, rngCount') <- evalShow rngCount i
  return (d ++ setToDropped (getDrop i' sk) ++ getKeep i' sk, rngCount')
  where
    (k, d) = separateKeptDropped is
    order l l' = if isLow lh then compare l l' else compare l' l
    sk = sortBy order k
    i = fromMaybe (Value 0) (getValueLowHigh lh)
    (getDrop, getKeep) = if kd == Keep then (genericDrop, genericTake) else (genericTake, genericDrop)

--- Pure evaluation functions for non-dice calculations
-- Was previously its own type class that wouldn't work for evaluating Base values.

binOpHelp :: (IOEval a, IOEval b, PrettyShow a, PrettyShow b) => Integer -> a -> b -> String -> (Integer -> Integer -> Integer) -> IO (Integer, String, Integer)
binOpHelp rngCount a b opS op = do
  (a', a's, rngCount') <- evalShow rngCount a
  (b', b's, rngCount'') <- evalShow rngCount' b
  return (op a' b', a's ++ " " ++ opS ++ " " ++ b's, rngCount'')

instance IOEval Expr where
  evalShow' rngCount (NoExpr t) = evalShow rngCount t
  evalShow' rngCount (Add t e) = binOpHelp rngCount t e "+" (+)
  evalShow' rngCount (Sub t e) = binOpHelp rngCount t e "-" (-)

instance IOEval Term where
  evalShow' rngCount (NoTerm f) = evalShow rngCount f
  evalShow' rngCount (Multi f t) = binOpHelp rngCount f t "*" (*)
  evalShow' rngCount (Div f t) = do
    (f', f's, rngCount') <- evalShow rngCount f
    (t', t's, rngCount'') <- evalShow rngCount' t
    if t' == 0
      then throwBot (EvaluationException "division by zero" [])
      else return (div f' t', f's ++ " / " ++ t's, rngCount'')

instance IOEval Func where
  evalShow' rngCount (Func "id" neg) = evalShow rngCount neg
  evalShow' rngCount (Func "fact" neg) = do
    (neg', neg's, rngCount') <- evalShow rngCount neg
    if neg' > factorialLimit
      then throwBot $ EvaluationException ("tried to evaluate a factorial with input number greater than the limit (" ++ show factorialLimit ++ "): `" ++ show neg' ++ "`") []
      else do
        f <- getFunc "fact"
        return (f neg', "fact" ++ " " ++ neg's, rngCount')
  evalShow' rngCount (Func s neg) = do
    (neg', neg's, rngCount') <- evalShow rngCount neg
    f <- getFunc s
    return (f neg', s ++ " " ++ neg's, rngCount')

instance IOEval Negation where
  evalShow' rngCount (Neg expo) = do
    (expo', expo's, rngCount') <- evalShow rngCount expo
    return (negate expo', "-" ++ expo's, rngCount')
  evalShow' rngCount (NoNeg expo) = evalShow rngCount expo

instance IOEval Expo where
  evalShow' rngCount (NoExpo b) = evalShow rngCount b
  evalShow' rngCount (Expo b expo) = do
    (expo', expo's, rngCount') <- evalShow rngCount expo
    if expo' < 0
      then throwBot (EvaluationException "the exponent is negative" [prettyShow expo])
      else do
        (b', b's, rngCount'') <- evalShow rngCount' b
        return (b' ^ expo', b's ++ " ^ " ++ expo's, rngCount'')

instance IOEval NumBase where
  evalShow' rngCount (Paren e) = do
    (r, s, rngCount') <- evalShow rngCount e
    return (r, "(" ++ s ++ ")", rngCount')
  evalShow' rngCount (Value i) = return (i, show i, rngCount)

--- Pretty printing the AST
-- The output from this should be parseable

-- | Type class to display an expression prettily (not neccessarily accurately).
class PrettyShow a where
  -- | Print the given value prettily.
  prettyShow :: a -> String

instance PrettyShow Expr where
  prettyShow (Add t e) = prettyShow t <> " + " <> prettyShow e
  prettyShow (Sub t e) = prettyShow t <> " - " <> prettyShow e
  prettyShow (NoExpr t) = prettyShow t

instance PrettyShow Term where
  prettyShow (Multi f t) = prettyShow f <> " * " <> prettyShow t
  prettyShow (Div f t) = prettyShow f <> " / " <> prettyShow t
  prettyShow (NoTerm f) = prettyShow f

instance PrettyShow Func where
  prettyShow (Func "id" n) = prettyShow n
  prettyShow (Func s n) = s <> " " <> prettyShow n

instance PrettyShow Negation where
  prettyShow (Neg expo) = "-" <> prettyShow expo
  prettyShow (NoNeg expo) = prettyShow expo

instance PrettyShow Expo where
  prettyShow (NoExpo b) = prettyShow b
  prettyShow (Expo b expo) = prettyShow b <> " ^ " <> prettyShow expo

instance PrettyShow NumBase where
  prettyShow (Paren e) = "(" <> prettyShow e <> ")"
  prettyShow (Value i) = fromString $ show i

instance PrettyShow Base where
  prettyShow (NBase nb) = prettyShow nb
  prettyShow (DiceBase dop) = prettyShow dop

instance PrettyShow Die where
  prettyShow (Die b) = "d" <> prettyShow b
  prettyShow (CustomDie is) = "d{" <> intercalate ", " (prettyShow <$> is) <> "}"
  prettyShow (LazyDie d) = "d!" ++ Prelude.tail (prettyShow d)

instance PrettyShow Dice where
  prettyShow (Dice b d dor) = prettyShow b <> prettyShow d <> helper' dor
    where
      fromOrdering ao = M.findWithDefault "??" ao $ snd advancedOrderingMapping
      fromLHW (Where o i) = "w" <> fromOrdering o <> prettyShow i
      fromLHW (Low i) = "l" <> fromString (prettyShow i)
      fromLHW (High i) = "h" <> fromString (prettyShow i)
      helper' Nothing = ""
      helper' (Just (DieOpRecur dopo' dor')) = helper dopo' <> helper' dor'
      helper (DieOpOptionLazy doo) = "!" <> helper doo
      helper (Reroll True o i) = "ro" <> fromOrdering o <> prettyShow i
      helper (Reroll False o i) = "rr" <> fromOrdering o <> prettyShow i
      helper (DieOpOptionKD Keep lhw) = "k" <> fromLHW lhw
      helper (DieOpOptionKD Drop lhw) = "d" <> fromLHW lhw

--- Parsing expressions below this line

instance CanParse Expr where
  pars = do
    t <- pars
    (try (skipSpace *> char '+') *> skipSpace *> (Add t <$> pars))
      <|> (try (skipSpace *> char '-') *> skipSpace *> (Sub t <$> pars))
      <|> (return . NoExpr) t

instance CanParse Term where
  pars = do
    t <- pars
    (try (skipSpace *> char '*') *> skipSpace *> (Multi t <$> pars))
      <|> (try (skipSpace *> char '/') *> skipSpace *> (Div t <$> pars))
      <|> (return . NoTerm) t

instance CanParse Func where
  pars = do
    funcName <- optional $ try ((pack <$> word) <* skipSpace1)
    t <- pars
    matchFuncName funcName t
    where
      matchFuncName Nothing t = return $ Func "id" t
      matchFuncName (Just s) t
        | unpack s `member` supportedFunctions = return $ Func (unpack s) t
        | otherwise = fail $ "could not find function with name `" ++ unpack s ++ "`"

instance CanParse Negation where
  pars =
    try (char '-') *> skipSpace *> (Neg <$> pars)
      <|> NoNeg <$> pars

instance CanParse Expo where
  pars = do
    t <- pars
    (try (skipSpace *> char '^') *> skipSpace *> (Expo t <$> pars))
      <|> (return . NoExpo) t

instance CanParse NumBase where
  pars =
    ( (try (skipSpace *> char '(') *> skipSpace *> (Paren . unnest <$> pars) <* skipSpace <* char ')')
        <|> try (Value <$> posInteger)
    )
      <?> "could not parse numBase (parentheses, an integer)"
    where
      unnest (NoExpr (NoTerm (Func "id" (NoNeg (NoExpo (NBase (Paren e))))))) = e
      unnest e = e

instance CanParse Base where
  pars =
    (try (DiceBase <$> pars) <|> try (NBase <$> pars))
      <?> "could not match a base token (dice, parentheses, an integer)"

instance CanParse Die where
  pars = do
    _ <- char 'd'
    lazyFunc <- (try (char '!') $> LazyDie) <|> return id
    ( try (lazyFunc . Die <$> pars)
        <|> lazyFunc . CustomDie
          <$> ( try (char '{' *> skipSpace)
                  *> (pars >>= (\i -> (i :) <$> many (try (skipSpace *> char ',' *> skipSpace) *> pars)))
                  <* skipSpace
                  <* char '}'
              )
      )
      <?> "recursed to die expression and could not find a die"

instance CanParse Dice where
  pars = do
    t <- optional $ try (pars :: Parser NumBase)
    bd <- parseDice'
    let t' = NBase $ fromMaybe (Value 1) t
    return $ bd t'

-- | Helper for parsing Dice, where as many `Dice` as possible are parsed and a function
-- that takes a `Base` value and returns a `Dice` value is returned. This `Base` value is
-- meant to be first value that `Dice` have.
parseDice' :: Parser (Base -> Dice)
parseDice' = do
  d <- pars :: Parser Die
  mdor <- parseDieOpRecur
  ( do
      bd <- try parseDice'
      return (\b -> bd (DiceBase $ Dice b d mdor))
    )
    <|> return (\b -> Dice b d mdor)

-- | Parse a `!=`, `<=`, `>=`, `<`, `=`, `>` as an `AdvancedOrdering`.
parseAdvancedOrdering :: Parser AdvancedOrdering
parseAdvancedOrdering = (try (string "!=" <|> string ">=" <|> string "<=" <|> string "<" <|> string "=" <|> string ">") <?> "could not parse an ordering") >>= matchO
  where
    matchO s = M.findWithDefault (fail "could not parse an ordering") s (M.map return $ fst advancedOrderingMapping)

-- | Parse a `LowHighWhere`, which is an `h` followed by an integer.
parseLowHigh :: Parser LowHighWhere
parseLowHigh = (try (char 'h' <|> char 'l' <|> char 'w') <?> "could not parse high, low, where") >>= helper
  where
    helper 'h' = High <$> pars
    helper 'l' = Low <$> pars
    helper 'w' = parseAdvancedOrdering >>= \o -> pars <&> Where o
    helper _ = fail "could not determine whether to keep/drop highest/lowest"

-- | Parse a bunch of die options.
parseDieOpRecur :: Parser (Maybe DieOpRecur)
parseDieOpRecur = do
  dopo <- optional (try parseDieOpOption)
  if isNothing dopo
    then return Nothing
    else do
      dor <- parseDieOpRecur
      return $ (DieOpRecur <$> dopo) <*> Just dor

-- | Parse a single die option.
parseDieOpOption :: Parser DieOpOption
parseDieOpOption = do
  lazyFunc <- (try (char '!') $> DieOpOptionLazy) <|> return id
  ( (try (string "ro") *> parseAdvancedOrdering >>= \o -> Reroll True o <$> pars)
      <|> (try (string "rr") *> parseAdvancedOrdering >>= \o -> Reroll False o <$> pars)
      <|> ((try (char 'k') *> parseLowHigh) <&> DieOpOptionKD Keep)
      <|> ((try (char 'd') *> parseLowHigh) <&> DieOpOptionKD Drop) <&> lazyFunc
    )
    <?> "could not parse dieOpOption - expecting one of the options described in the doc (call `help roll` to access)"
