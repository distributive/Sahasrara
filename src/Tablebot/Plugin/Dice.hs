-- |
-- Module      : Tablebot.Plugin.Dice
-- Description : Lex and parse dice using this plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the neccessary parsers and stucture to get the AST for an
-- expression that contains dice
module Tablebot.Plugin.Dice (evalExpr, Expr, PrettyShow (..), supportedFunctionsList) where

import Control.Monad (when)
import Control.Monad.Exception (MonadException)
import Data.Functor ((<&>))
import Data.List (genericDrop, genericReplicate, genericTake, sortBy)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), head, tail, (<|))
import Data.Map as M (Map, findWithDefault, fromList, keys, map, member, (!))
import Data.Maybe (fromMaybe, isNothing)
import Data.Set as S (Set, fromList, singleton, toList, unions)
import Data.Text (pack, unpack)
import Safe.Foldable (maximumMay, minimumMay)
import System.Random (Random (randomRIO))
import Tablebot.Plugin.Exception (BotException (EvaluationException), throwBot)
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
  when (i > maximumRNG) $ throwBot $ EvaluationException $ "exceeded maximum rng count (" ++ show maximumRNG ++ "). rng count reached: " ++ show i

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

-- | The type representing a simple N sided die or a custom die.
data Die = Die NumBase | CustomDie [Integer] deriving (Show, Eq)

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
  = Reroll {rerollOnce :: Bool, condition :: Ordering, limit :: Integer}
  | DieOpOptionKD KeepDrop LowHighWhere
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
getFunc s = M.findWithDefault (throwBot $ EvaluationException $ "could not find function `" ++ s ++ "`") s (M.map return supportedFunctions)

--- Evaluating an expression. Uses IO because dice are random

-- | Given an expression, evaluate it, getting the pretty printed string and the value of
-- the result
evalExpr :: Expr -> IO (Integer, String, Integer)
evalExpr = evalShow

-- | Utility function to display dice.
--
-- The tuple of integers denotes what the critvalues of this dice value are. The `a`
-- denotes the value that is being printed, and needs to have `PrettyShow` defined for it.
-- Finally, the list of tuples denotes all the values that the `a` value has gone through.
-- If the `Maybe Bool` value is `Nothing`, the number is displayed as normal. If the value
-- is `Just False`, the value has been rerolled over, and is displayed crossed out. If the
-- value is `Just True`, the value has been dropped, and the number is crossed out and
-- underlined.
dieShow :: (PrettyShow a, MonadException m) => (Integer, Integer) -> a -> [(Integer, Maybe Bool)] -> m String
dieShow _ _ [] = throwBot $ EvaluationException "tried to show empty set of results"
dieShow (lc, hc) d ls = return $ prettyShow d ++ " [" ++ foldl1 (\rst n -> rst ++ ", " ++ n) adjustList ++ "]"
  where
    toCrit i
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
  -- dice should be displayed.
  evalShow :: a -> IO (Integer, String, Integer)

instance IOEval Base where
  evalShow (NBase nb) = evalShow nb
  evalShow (DiceBase dice) = evalShow dice

instance IOEval Die where
  evalShow d@(CustomDie is) = do
    i <- chooseOne is
    ds <- dieShow (minimum is, maximum is) d [(i, Nothing)]
    return (i, ds, 1)
  evalShow d@(Die b) = do
    (bound, _, rngCount) <- evalShow b
    checkRNGCount rngCount
    if bound < 1
      then throwBot $ EvaluationException $ "Cannot roll a < 1 sided die (`" ++ prettyShow b ++ "`)"
      else do
        i <- randomRIO (1, bound)
        ds <- dieShow (1, maxVal b) d [(i, Nothing)]
        checkRNGCount (rngCount + 1)
        return (i, ds, 1 + rngCount)

instance IOEval Dice where
  evalShow dop = do
    (lst, rng, rngCount) <- evalDieOp dop
    checkRNGCount rngCount
    let vs = fromEvalDieOpList lst
    s <- dieShow (minimum rng, maximum rng) dop vs
    return (sum (fst <$> filter (isNothing . snd) vs), s, rngCount)

-- | Utility function to transform the output list type of other utility functions into
-- one that `dieShow` recognises
fromEvalDieOpList :: [(NonEmpty Integer, Bool)] -> [(Integer, Maybe Bool)]
fromEvalDieOpList = foldr foldF []
  where
    foldF (is, b) lst = let is' = (,Just False) <$> NE.tail is in ((NE.head is, if b then Nothing else Just True) : is' ++ lst)

-- | Helper function that takes a set of Dice and returns a tuple of two items. The second
-- item is the range of the base die. The first item is a list representing each die - a
-- tuple with a history of the die being rolled, and whether the die has been dropped or
-- not. The first item of each die record is the current value of the die.
--
-- The function itself checks to make sure the number of dice being rolleed is less than
-- the maximum recursion and is non-negative.
evalDieOp :: Dice -> IO ([(NonEmpty Integer, Bool)], [Integer], Integer)
evalDieOp (Dice b ds dopo) = do
  (nbDice, _, rngCountb) <- evalShow b
  checkRNGCount rngCountb
  if nbDice >= maximumRNG
    then throwBot (EvaluationException $ "tried to roll more than " ++ show maximumRNG ++ " dice")
    else do
      if nbDice < 0
        then throwBot (EvaluationException "tried to give a negative value to the number of dice")
        else do
          rolls <- mapM (fmap mp . evalShow) (genericReplicate nbDice ds)
          let (vs, rng) = (fmap (\(i, rngcnt) -> (i :| [], True, rngcnt)) rolls, range ds)
          rs <- evalDieOp' dopo rng vs
          let (rs', rngCount) = foldr (\(is, bo, cnt) (rs'', rngCount') -> ((is, bo) : rs'', cnt + rngCount')) ([], rngCountb) rs
          checkRNGCount (rngCount)
          return (rs', rng, rngCount)
  where
    mp (a, _, a') = (a, a')

-- | Utility function that processes a `Maybe DieOpRecur`, when given a range for dice,
-- and dice that have already been processed.
evalDieOp' :: Maybe DieOpRecur -> [Integer] -> [(NonEmpty Integer, Bool, Integer)] -> IO [(NonEmpty Integer, Bool, Integer)]
evalDieOp' Nothing _ is = return is
evalDieOp' (Just (DieOpRecur doo mdor)) rng is = do
  is' <- evalDieOp'' doo rng is
  checkRNGCount $ sum $ fmap thrd is'
  evalDieOp' mdor rng is'
  where
    thrd (_, _, i) = i

-- | Utility function that processes a `DieOpOption`, when given a range for dice,
-- and dice that have already been processed.
evalDieOp'' :: DieOpOption -> [Integer] -> [(NonEmpty Integer, Bool, Integer)] -> IO [(NonEmpty Integer, Bool, Integer)]
evalDieOp'' (DieOpOptionKD kd lhw) _ is = return $ evalDieOpHelpKD kd lhw is
evalDieOp'' (Reroll once o i) rng is = mapM rerollF is
  where
    rerollF g@(i', b, rngCount) =
      if b && compare (NE.head i') i == o
        then do
          v <- chooseOne rerollRange
          return (v <| i', b, rngCount + 1)
        else return g
    rerollRange = if not once then filter (\i' -> o /= compare i' i) rng else rng

-- | Given a list of dice values, separate them into kept values and dropped values
-- respectively.
separateKeptDropped :: [(NonEmpty Integer, Bool, Integer)] -> ([(NonEmpty Integer, Bool, Integer)], [(NonEmpty Integer, Bool, Integer)])
separateKeptDropped = foldr f ([], [])
  where
    f a@(_, True, _) (kept, dropped) = (a : kept, dropped)
    f a@(_, False, _) (kept, dropped) = (kept, a : dropped)

-- | Utility function to set all the values in the given list to be dropped.
setToDropped :: [(NonEmpty Integer, Bool, Integer)] -> [(NonEmpty Integer, Bool, Integer)]
setToDropped = fmap (\(is, _, cnt) -> (is, False, cnt))

-- TODO: make the keep/drop on low/high not require a sort somehow, or if it does to not change the output order of the values

-- | Helper function that executes the keep/drop commands on dice.
evalDieOpHelpKD :: KeepDrop -> LowHighWhere -> [(NonEmpty Integer, Bool, Integer)] -> [(NonEmpty Integer, Bool, Integer)]
evalDieOpHelpKD Keep (Where cmp i) is = fmap (\(iis, b, cnt) -> (iis, b && compare (NE.head iis) i == cmp, cnt)) is
evalDieOpHelpKD Drop (Where cmp i) is = fmap (\(iis, b, cnt) -> (iis, b && compare (NE.head iis) i /= cmp, cnt)) is
evalDieOpHelpKD kd lh is = d ++ setToDropped (getDrop i sk) ++ getKeep i sk
  where
    (k, d) = separateKeptDropped is
    order l l' = if isLow lh then compare l l' else compare l' l
    sk = sortBy order k
    i = fromMaybe 0 (getValueLowHigh lh)
    (getDrop, getKeep) = if kd == Keep then (genericDrop, genericTake) else (genericTake, genericDrop)

--- Pure evaluation functions for non-dice calculations
-- Was previously its own type class that wouldn't work for evaluating Base values.
instance IOEval Expr where
  evalShow (NoExpr t) = evalShow t
  evalShow (Add t e) = do
    (t', t's, rngCount) <- evalShow t
    (e', e's, rngCount') <- evalShow e
    checkRNGCount (rngCount + rngCount')
    return (t' + e', t's ++ " + " ++ e's, rngCount + rngCount')
  evalShow (Sub t e) = do
    (t', t's, rngCount) <- evalShow t
    (e', e's, rngCount') <- evalShow e
    checkRNGCount (rngCount + rngCount')
    return (t' - e', t's ++ " - " ++ e's, rngCount + rngCount')

instance IOEval Term where
  evalShow (NoTerm f) = evalShow f
  evalShow (Multi f t) = do
    (f', f's, rngCount) <- evalShow f
    (t', t's, rngCount') <- evalShow t
    checkRNGCount (rngCount + rngCount')
    return (f' * t', f's ++ " * " ++ t's, rngCount + rngCount')
  evalShow (Div f t) = do
    (f', f's, rngCount) <- evalShow f
    (t', t's, rngCount') <- evalShow t
    checkRNGCount (rngCount + rngCount')
    if t' == 0
      then throwBot (EvaluationException "division by zero")
      else return (div f' t', f's ++ " / " ++ t's, rngCount + rngCount')

instance IOEval Func where
  evalShow (Func "id" neg) = evalShow neg
  evalShow (Func "fact" neg) = do
    (neg', neg's, rngCount) <- evalShow neg
    if neg' > factorialLimit
      then throwBot $ EvaluationException $ "tried to evaluate a factorial with input number greater than the limit: `" ++ show neg' ++ "`"
      else do
        f <- getFunc "fact"
        return (f neg', "fact" ++ " " ++ neg's, rngCount)
  evalShow (Func s neg) = do
    (neg', neg's, rngCount) <- evalShow neg
    f <- getFunc s
    return (f neg', s ++ " " ++ neg's, rngCount)

instance IOEval Negation where
  evalShow (Neg expo) = do
    (expo', expo's, rngCount) <- evalShow expo
    return (negate expo', "-" ++ expo's, rngCount)
  evalShow (NoNeg expo) = evalShow expo

instance IOEval Expo where
  evalShow (NoExpo b) = evalShow b
  evalShow (Expo b expo) = do
    (expo', expo's, rngCount) <- evalShow expo
    if expo' < 0
      then throwBot (EvaluationException "the exponent is negative")
      else do
        (b', b's, rngCount') <- evalShow b
        checkRNGCount (rngCount + rngCount')
        return (b' ^ expo', b's ++ " ^ " ++ expo's, rngCount + rngCount')

instance IOEval NumBase where
  evalShow (Paren e) = do
    (r, s, rngCount) <- evalShow e
    return (r, "(" ++ s ++ ")", rngCount)
  evalShow (Value i) = return (i, show i, 0)

--- Finding the range of an expression.

-- TODO: make range return all possible values, repeated the number of times they would be
-- present (so it can be used for statistics)

-- | Type class to find the range and bounds of a given value.
class Range a where
  range :: a -> [Integer]
  range = toList . range'
  range' :: a -> Set Integer
  maxVal :: a -> Integer
  minVal :: a -> Integer

instance Range Expr where
  range' (Add t e) = S.fromList $ ((+) <$> range t) <*> range e
  range' (Sub t e) = S.fromList $ ((-) <$> range t) <*> range e
  range' (NoExpr t) = range' t
  maxVal (Add t e) = maxVal t + maxVal e
  maxVal (Sub t e) = maxVal t - minVal e
  maxVal (NoExpr t) = maxVal t
  minVal (Add t e) = minVal t + minVal e
  minVal (Sub t e) = minVal t - maxVal e
  minVal (NoExpr t) = minVal t

instance Range Term where
  range' (Multi f t) = S.fromList $ ((*) <$> range f) <*> range t
  range' (Div f t) = S.fromList $ (div <$> range f) <*> filter (/= 0) (range t)
  range' (NoTerm f) = range' f
  maxVal (Multi f t) = maxVal f * maxVal t
  maxVal (Div f t) = maxVal f `div` minVal t
  maxVal (NoTerm f) = maxVal f
  minVal (Multi f t) = minVal f * minVal t
  minVal (Div f t) = minVal f `div` maxVal t
  minVal (NoTerm f) = minVal f

-- NOTE: this is unsafe since the function requested may not be defined
-- if using the dice parser functions, it'll be safe, but for all other uses, beware
instance Range Func where
  range' (Func s n) = S.fromList $ (supportedFunctions M.! s) <$> range n
  maxVal (Func "id" n) = maxVal n
  maxVal f = maximum (range f)
  minVal (Func "id" n) = minVal n
  minVal f = minimum (range f)

instance Range Negation where
  range' (Neg expo) = S.fromList $ negate <$> range expo
  range' (NoNeg expo) = range' expo
  maxVal (NoNeg expo) = maxVal expo
  maxVal (Neg expo) = negate $ minVal expo
  minVal (NoNeg expo) = minVal expo
  minVal (Neg expo) = negate $ maxVal expo

instance Range Expo where
  range' (NoExpo b) = range' b
  range' (Expo b expo) = S.fromList $ ((^) <$> range b) <*> range expo
  maxVal (NoExpo b) = maxVal b
  maxVal (Expo b expo) = maxVal b ^ maxVal expo
  minVal (NoExpo b) = minVal b
  minVal (Expo b expo) = minVal b ^ minVal expo

instance Range NumBase where
  range' (Value i) = singleton i
  range' (Paren e) = range' e
  maxVal (Value i) = i
  maxVal (Paren e) = maxVal e
  minVal (Value i) = i
  minVal (Paren e) = minVal e

instance Range Base where
  range' (NBase nb) = range' nb
  range' (DiceBase dop) = range' dop
  maxVal (NBase nb) = maxVal nb
  maxVal (DiceBase dop) = maxVal dop
  minVal (NBase nb) = minVal nb
  minVal (DiceBase dop) = minVal dop

instance Range Die where
  range' (CustomDie is) = S.fromList is
  range' (Die b) = S.fromList [1 .. (maxVal b)]
  maxVal (CustomDie is) = maximum is
  maxVal (Die b) = maxVal b
  minVal (CustomDie is) = minimum is
  minVal (Die _) = 1

-- instance Range MultiDie where
--   range' (SingleDie d) = range' d
--   range' (MultiDie nb d) = S.fromList $ do
--     i <- range nb
--     foldr (\a b -> ((+) <$> a) <*> b) [0] $ genericReplicate i (range d)

--   maxVal (SingleDie d) = maxVal d
--   maxVal (MultiDie nb d) = maxVal nb * maxVal d
--   minVal (SingleDie d) = maxVal d
--   minVal (MultiDie nb d) = max 0 (minVal nb) * minVal d

-- TODO: check this more
instance Range Dice where
  range' d = S.unions $ fmap foldF counts
    where
      (counts, dr) = diceVals d
      foldF' i js
        | i < 1 = []
        | i == 1 = js
        | otherwise = ((+) <$> dr) <*> foldF' (i - 1) js
      foldF i = S.fromList $ foldF' i dr
  maxVal d
    | mxdr < 0 = fromMaybe 0 (minimumMay counts) * mxdr
    | otherwise = fromMaybe 0 (maximumMay counts) * mxdr
    where
      (counts, dr) = diceVals d
      mxdr = fromMaybe 0 $ maximumMay dr
  minVal d
    | mndr < 0 = fromMaybe 0 (maximumMay counts) * mndr
    | otherwise = fromMaybe 0 (minimumMay counts) * mndr
    where
      (counts, dr) = diceVals d
      mndr = fromMaybe 0 $ minimumMay dr

type DieRange = [Integer]

-- the tuple is the range of the number of dice, the current die range, and the total die range possible with the dice being used

-- | Applies a given die operation to the current die ranges. The tuple given and returned
-- represents the number of dice, the current range of the die, and the base die range.
applyDieOpVal :: DieOpOption -> ([Integer], DieRange, DieRange) -> ([Integer], DieRange, DieRange)
applyDieOpVal (Reroll ro c l) t@(is, cdr, dr)
  | any boolF cdr = (is, applyBoolF dr, dr)
  | otherwise = t
  where
    boolF i' = compare i' l == c
    applyBoolF = if ro then id else filter (not . boolF)
applyDieOpVal (DieOpOptionKD kd (Where o i)) (is, cdr, dr)
  | any boolF cdr = ([0 .. maximum is], filter boolF cdr, dr)
  | otherwise = (is, cdr, dr)
  where
    boolF i' = (if kd == Keep then id else not) $ compare i' i == o
applyDieOpVal (DieOpOptionKD kd lh) (is, cdr, dr) = (f (getValueLowHigh lh) <$> is, cdr, dr)
  where
    f (Just i) i' = if kd == Keep then min i i' else max 0 (i' - i)
    f Nothing i' = i'

-- | Get the number of dice and the die range of a given set of dice.
diceVals :: Dice -> ([Integer], DieRange)
diceVals (Dice b d mdor) = (filter (>= 0) counts, dr)
  where
    (counts, dr, _) = diceVals' mdor (filter (>= 0) (range b), dieVals, dieVals)
    dieVals = range d

-- | Helper function to iterate through all the `DieOpOption`s for a give set of dice.
diceVals' :: Maybe DieOpRecur -> ([Integer], DieRange, DieRange) -> ([Integer], DieRange, DieRange)
diceVals' Nothing t = t
diceVals' (Just (DieOpRecur doo mdor)) t = diceVals' mdor (applyDieOpVal doo t)

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

-- TODO: better way to display custom die
instance PrettyShow Die where
  prettyShow (Die b) = "d" <> prettyShow b
  prettyShow (CustomDie is) = "d{" <> (init . Prelude.tail . fromString . show) is <> "}"

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
    try (Die <$> pars)
      <|> CustomDie
        <$> ( try (char '{' *> skipSpace)
                *> (integer >>= (\i -> (i :) <$> many (try (skipSpace *> char ',' *> skipSpace) *> integer)))
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

-- | Parse a `LowHighWhere`, which is an `h` followed by a positive integer.
parseLowHigh :: Parser LowHighWhere
parseLowHigh = (char 'h' <|> char 'l' <|> char 'w') >>= helper
  where
    helper 'h' = High <$> posInteger
    helper 'l' = Low <$> posInteger
    helper 'w' = parseOrdering >>= \o -> posInteger <&> Where o
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
  (try (string "ro") *> parseOrdering >>= \o -> Reroll True o <$> posInteger)
    <|> (try (string "rr") *> parseOrdering >>= \o -> Reroll False o <$> posInteger)
    <|> ((try (char 'k') *> parseLowHigh) <&> DieOpOptionKD Keep)
    <|> ((try (char 'd') *> parseLowHigh) <&> DieOpOptionKD Drop)
    <|> fail "could not parse dieOpOption"
