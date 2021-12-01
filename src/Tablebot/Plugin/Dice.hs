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
module Tablebot.Plugin.Dice (evalExpr, Expr, PrettyShow (..), supportedFunctionsList) where

import Control.Monad (when)
import Control.Monad.Exception (MonadException)
import Data.Functor ((<&>), ($>))
import Data.List (genericDrop, genericReplicate, genericTake, sortBy, intercalate)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, tail, (<|))
import Data.Map as M (Map, findWithDefault, fromList, keys, map, member)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (pack, unpack)
import System.Random (Random (randomRIO))
import Tablebot.Plugin.Exception (BotException (EvaluationException), catchBot, throwBot)
import Tablebot.Plugin.Parser (integer, posInteger, skipSpace, skipSpace1, word)
import Tablebot.Plugin.Random (chooseOne)
import Tablebot.Plugin.SmartCommand (CanParse (..), FromString (fromString))
import Tablebot.Plugin.Types (Parser)
import Text.Megaparsec (MonadParsec (try), many, optional, (<|>))
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

checkRNGCount :: Integer -> IO ()
checkRNGCount i =
  when (i > maximumRNG) $ throwBot $ EvaluationException ("exceeded maximum rng count (" ++ show maximumRNG ++ ")") []

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

-- Dice Operations after this point

-- TODO: create a lazy and a repeated eval die so that sometimes dice quantities are eval'd
-- each time and sometimes are eval'd once and then that value is used

-- | The type representing a simple N sided die or a custom die.
data Die = Die NumBase | CustomDie [Base] | LazyDie Die deriving (Show, Eq)

-- | The type representing a number of dice equal to the `Base` value, and possibly some
-- die options.
data Dice = Dice Base Die (Maybe DieOpRecur)
  deriving (Show, Eq)

-- | The type representing one or more die options.
data DieOpRecur = DieOpRecur DieOpOption (Maybe DieOpRecur)
  deriving (Show, Eq)

-- TODO: change rerolls and LowHighWhere to use NumBase instead of Integer... even if it
-- makes my life harder.

-- | The type representing a die option.
data DieOpOption
  = Reroll {rerollOnce :: Bool, condition :: Ordering, limit :: NumBase}
  | DieOpOptionKD KeepDrop LowHighWhere
  | DieOpOptionLazy DieOpOption
  deriving (Show, Eq)

-- | A type used to designate how the keep/drop option should work
data LowHighWhere = Low Integer | High Integer | Where Ordering Integer deriving (Show, Eq)

-- | Utility function to get the integer determining how many values to get given a
-- `LowHighWhere`. If the given value is `Low` or `High`, then Just the integer contained
-- is returned. Else, Nothing is returned.
getValueLowHigh :: LowHighWhere -> Maybe Integer
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
      ("negate", negate)
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
    toCrit = if isNothing lchc
      then show
      else toCrit'
    (lc,hc) = fromMaybe (0,0) lchc
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
    (i,_,rngCount') <- evalShow rngCount d
    ds <- dieShow Nothing ld [(i, Nothing)]
    return (i,ds,rngCount')
  evalShow' rngCount d@(CustomDie is) = do
    i <- chooseOne is
    (i' , _, rngCount') <- evalShow rngCount i
    ds <- dieShow Nothing d [(i', Nothing)]
    checkRNGCount (rngCount' + 1)
    return (i', ds, rngCount' + 1)
  evalShow' rngCount d@(Die b) = do
    (bound, _, rngCount') <- evalShow rngCount b
    if bound < 1
      then throwBot $ EvaluationException ("Cannot roll a < 1 sided die (`" ++ prettyShow b ++ "`)") []
      else do
        let dBounds = (1,bound)
        i <- randomRIO dBounds
        ds <- dieShow Nothing d [(i, Nothing)]
        checkRNGCount (rngCount' + 1)
        return (i, ds, rngCount' + 1)

instance IOEval Dice where
  evalShow' rngCount dop = do
    (lst, mnmx, rngCount') <- evalDieOp dop rngCount
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
evalDieOp :: Dice -> Integer -> IO ([(NonEmpty Integer, Bool)], Maybe (Integer, Integer), Integer)
evalDieOp (Dice b ds dopo) rngCount = do
  (nbDice, _, rngCountB) <- evalShow rngCount b
  if nbDice >= maximumRNG
    then throwBot (EvaluationException ("tried to roll more than " ++ show maximumRNG ++ " dice: " ++ show nbDice) [prettyShow b])
    else do
      if nbDice < 0
        then throwBot (EvaluationException ("tried to give a negative value to the number of dice: " ++ show nbDice) [prettyShow b])
        else do
          (ds', rngCountCondense, crits) <- condenseDie rngCountB ds
          (rolls, rngCountRolls) <- foldr foldF (return ([], rngCountCondense)) (genericReplicate nbDice ds')
          let vs = fmap (\i -> (i :| [], True)) rolls
          (rs, rngCountRs) <- evalDieOp' dopo ds' rngCountRolls vs
          return (sortBy sortByOption rs, crits, rngCountRs)
  where
    condenseDie rngCount' (Die dBase) = do
      (i, _, rngCount'') <- evalShow rngCount' dBase
      return (Die (Value i), rngCount'', Just (1, i))
    condenseDie rngCount' (CustomDie is) = do
      (is', rngCount'') <- foldr foldF (return ([],rngCount')) is
      return (CustomDie (NBase . Value <$> is'), rngCount'', Nothing)
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
evalDieOp' :: Maybe DieOpRecur -> Die -> Integer -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOp' Nothing _ rngCount is = return (is, rngCount)
evalDieOp' (Just (DieOpRecur doo mdor)) die rngCount is = do
  (is', rngCount') <- evalDieOp'' doo die rngCount is
  evalDieOp' mdor die rngCount' is'

-- | Utility function that processes a `DieOpOption`, when given a range for dice,
-- and dice that have already been processed.
evalDieOp'' :: DieOpOption -> Die -> Integer -> [(NonEmpty Integer, Bool)] -> IO ([(NonEmpty Integer, Bool)], Integer)
evalDieOp'' (DieOpOptionKD kd lhw) _ rngCount is = return (evalDieOpHelpKD kd lhw is, rngCount)
evalDieOp'' (Reroll once o i) die rngCount is = foldr rerollF (return ([], rngCount)) is
  where
    rerollF g@(i', b) isRngCount' = do
      (is', rngCount') <- isRngCount'
      if b && compare (NE.head i') i == o
        then do
          (v, _, rngCount'') <- evalShow rngCount' die
          let ret = (v <| i', b)
          if once
            then return (ret : is', rngCount'')
            else rerollF ret (return (is', rngCount''))
        else return (g : is', rngCount')

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

-- TODO: make the keep/drop on low/high not require a sort somehow, or if it does to not change the output order of the values

-- | Helper function that executes the keep/drop commands on dice.
evalDieOpHelpKD :: KeepDrop -> LowHighWhere -> [(NonEmpty Integer, Bool)] -> [(NonEmpty Integer, Bool)]
evalDieOpHelpKD Keep (Where cmp i) is = fmap (\(iis, b) -> (iis, b && compare (NE.head iis) i == cmp)) is
evalDieOpHelpKD Drop (Where cmp i) is = fmap (\(iis, b) -> (iis, b && compare (NE.head iis) i /= cmp)) is
evalDieOpHelpKD kd lh is = d ++ setToDropped (getDrop i sk) ++ getKeep i sk
  where
    (k, d) = separateKeptDropped is
    order l l' = if isLow lh then compare l l' else compare l' l
    sk = sortBy order k
    i = fromMaybe 0 (getValueLowHigh lh)
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
      then throwBot $ EvaluationException ("tried to evaluate a factorial with input number greater than the limit: `" ++ show neg' ++ "`") []
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
      fromOrdering LT i = "<" <> fromString (show i)
      fromOrdering EQ i = "=" <> fromString (show i)
      fromOrdering GT i = ">" <> fromString (show i)
      fromLHW (Where o i) = "w" <> fromOrdering o i
      fromLHW (Low i) = "l" <> fromString (show i)
      fromLHW (High i) = "h" <> fromString (show i)
      helper' Nothing = ""
      helper' (Just (DieOpRecur dopo' dor')) = helper dopo' <> helper' dor'
      helper (Reroll True o i) = "ro" <> fromOrdering o i
      helper (Reroll False o i) = "rr" <> fromOrdering o i
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
        | unpack s `member` supportedFunctions = (return . Func (unpack s)) t
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
    (try (skipSpace *> char '(') *> skipSpace *> (Paren <$> pars) <* skipSpace <* char ')')
      <|> try (Value <$> posInteger)
      <|> fail "could not parse numBase"

instance CanParse Base where
  pars =
    try (DiceBase <$> pars)
      <|> try (NBase <$> pars)
      <|> fail "Could not match a base token"

instance CanParse Die where
  pars = do
    _ <- char 'd'
    lazyFunc <- (try (char '!') $>  LazyDie) <|> return id
    try (lazyFunc . Die <$> pars)
      <|> lazyFunc . CustomDie
        <$> ( try (char '{' *> skipSpace)
                *> (pars >>= (\i -> (i :) <$> many (try (skipSpace *> char ',' *> skipSpace) *> pars)))
                <* skipSpace
                <* char '}'
            )
      <|> fail "recursed to die expression and could not find a die"

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

-- | Parse a `<`, `=`, or `>` as an `Ordering`.
parseOrdering :: Parser Ordering
parseOrdering = (char '<' <|> char '=' <|> char '>') >>= matchO
  where
    matchO '<' = return LT
    matchO '=' = return EQ
    matchO '>' = return GT
    matchO _ = fail "tried to get an ordering that didn't exist"

-- | Parse a `LowHighWhere`, which is an `h` followed by an integer.
parseLowHigh :: Parser LowHighWhere
parseLowHigh = (char 'h' <|> char 'l' <|> char 'w') >>= helper
  where
    helper 'h' = High <$> integer
    helper 'l' = Low <$> integer
    helper 'w' = parseOrdering >>= \o -> integer <&> Where o
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
  (try (string "ro") *> parseOrdering >>= \o -> Reroll True o <$> integer)
    <|> (try (string "rr") *> parseOrdering >>= \o -> Reroll False o <$> integer)
    <|> ((try (char 'k') *> parseLowHigh) <&> DieOpOptionKD Keep)
    <|> ((try (char 'd') *> parseLowHigh) <&> DieOpOptionKD Drop)
    <|> fail "could not parse dieOpOption"
