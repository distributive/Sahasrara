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
module Tablebot.Plugin.Dice (evalExpr, Expr, PrettyShow (..)) where

import Control.Monad.Exception (MonadException)
import Data.Functor ((<&>))
import Data.List (genericDrop, genericReplicate, genericTake, sortBy)
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import Data.Map as M (Map, findWithDefault, fromList, map, member, (!))
import Data.Maybe (fromMaybe, isNothing)
import Data.Set as S (Set, fromList, singleton, toList, unions)
import Data.Text (pack, unpack)
import Safe.Foldable (maximumMay, minimumMay)
import System.Random (Random (randomRIO))
import Tablebot.Plugin.Exception (BotException (EvaluationException), throwBot)
import Tablebot.Plugin.Parser (posInteger, skipSpace, skipSpace1, word)
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
maximumRecursion :: Integer
maximumRecursion = 150

-- TODO: full check over of bounds. make this thing AIR TIGHT.

-- | The type of the top level expression. Represents one of addition, subtraction, or a
-- single term.
data Expr = Add Term Expr | Sub Term Expr | NoExpr Term
  deriving (Show, Eq)

-- | The type representing multiplication, division, or a single function application.
data Term = Multi Func Term | Div Func Term | NoTerm Func
  deriving (Show, Eq)

-- | The type representing a single function application on a negated item.
data Func = Id Negation | Abs Negation
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
data Base = NBase NumBase | DieOpBase DieOp
  deriving (Show, Eq)

-- Dice Operations after this point

-- | The type representing a simple N sided die or a custom die.
data Die = Die Base | CustomDie [Integer] deriving (Show, Eq)

-- | The type representing either multiple dice or a single die.
data MultiDie = MultiDie NumBase Die | SingleDie Die
  deriving (Show, Eq)

getDie :: MultiDie -> Die
getDie (SingleDie d) = d
getDie (MultiDie _ d) = d

getNumberOfDice :: MultiDie -> NumBase
getNumberOfDice (SingleDie _) = Value 1
getNumberOfDice (MultiDie nb _) = nb

-- | The type representing multiple dice or dice being modified by some die op option.
data DieOp
  = DieOp DieOp DieOpOption
  | DieOpMulti MultiDie
  deriving (Show, Eq)

-- | The type representing a die op option.
data DieOpOption
  = Reroll {rerollOnce :: Bool, condition :: Ordering, limit :: Integer}
  | DieOpOptionKD KeepDrop LowHighWhere
  deriving (Show, Eq)

data LowHighWhere = Low Integer | High Integer | Where Ordering Integer deriving (Show, Eq)

getValueLowHigh :: LowHighWhere -> Maybe Integer
getValueLowHigh (Low i) = Just i
getValueLowHigh (High i) = Just i
getValueLowHigh (Where _ _) = Nothing

isLow :: LowHighWhere -> Bool
isLow (Low _) = True
isLow _ = False

isHigh :: LowHighWhere -> Bool
isHigh (High _) = True
isHigh _ = False

data KeepDrop = Keep | Drop deriving (Show, Eq)

-- Mappings for what functions are supported
-- TODO: come up with a way that this can be used with function details so less has to be
-- repeated
supportedFunctions :: Map String (Negation -> Func, Integer -> Integer)
supportedFunctions =
  M.fromList
    [ ("abs", (Abs, abs)),
      ("id", (Id, id))
    ]

getFunc :: MonadException m => String -> m (Negation -> Func, Integer -> Integer)
getFunc s = M.findWithDefault (throwBot $ EvaluationException $ "could not find function `" ++ s ++ "`") s (M.map return supportedFunctions)

functionDetails :: Func -> (String, Negation)
functionDetails (Id a) = ("id", a)
functionDetails (Abs a) = ("abs", a)

--- Evaluating an expression. Uses IO because dice are random

evalExpr :: Expr -> IO (Integer, String)
evalExpr = evalShow

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

class IOEval a where
  evalShow :: a -> IO (Integer, String)

instance IOEval Base where
  evalShow (NBase nb) = evalShow nb
  evalShow (DieOpBase dop) = evalShow dop

instance IOEval Die where
  evalShow d@(CustomDie is) = do
    if length is >= fromIntegral maximumRecursion
      then throwBot (EvaluationException $ "tried to roll a custom die with more than " ++ show maximumRecursion ++ " sides")
      else do
        i <- chooseOne is
        ds <- dieShow (minimum is, maximum is) d [(i, Nothing)]
        return (i, ds)
  evalShow d@(Die b) = do
    (bound, _) <- evalShow b
    if bound >= maximumRecursion
      then throwBot (EvaluationException $ "tried to roll a die with more than " ++ show maximumRecursion ++ " sides")
      else do
        if bound < 1
          then throwBot $ EvaluationException $ "Cannot roll a < 1 sided die (`" ++ prettyShow b ++ "`)"
          else do
            i <- randomRIO (1, bound)
            ds <- dieShow (1, maxVal b) d [(i, Nothing)]
            return (i, ds)

instance IOEval MultiDie where
  evalShow (SingleDie d) = evalShow d
  evalShow md@(MultiDie nb d) = do
    (dieNum, _) <- evalShow nb
    if dieNum >= maximumRecursion
      then throwBot (EvaluationException $ "tried to roll more than " ++ show maximumRecursion ++ " dice")
      else do
        if dieNum < 0
          then throwBot (EvaluationException "tried to give a negative value to the number of dice")
          else do
            ds <- sequence $ genericReplicate dieNum $ fst <$> evalShow d
            dieshow <- dieShow (minVal d, maxVal d) md (fmap (,Nothing) ds)
            return (sum ds, dieshow)

instance IOEval DieOp where
  evalShow dop = do
    (lst, rng) <- evalDieOp dop
    let vs = fromEvalDieOpList lst
    s <- dieShow (minimum rng, maximum rng) dop vs
    return (sum (fst <$> filter (isNothing . snd) vs), s)

fromEvalDieOpList :: [(NonEmpty Integer, Bool)] -> [(Integer, Maybe Bool)]
fromEvalDieOpList = foldr foldF []
  where
    foldF (is, b) lst = let is' = (,Just False) <$> NE.tail is in (NE.head is,if b then Nothing else Just True) : is' ++ lst

evalDieOp :: DieOp -> IO ([(NonEmpty Integer, Bool)], [Integer])
evalDieOp (DieOpMulti md) = do
  (nbDice, _) <- evalShow (getNumberOfDice md)
  rolls <- mapM (\d -> evalShow d <&> fst) (genericReplicate nbDice (getDie md))
  return (fmap (\i -> (i NE.:| [], True)) rolls, range $ getDie md)
evalDieOp (DieOp dop dopo) = do
  (vs, rng) <- evalDieOp dop
  rs <- evalDieOp' dopo rng vs
  return (rs, rng)

evalDieOp' :: DieOpOption -> [Integer] -> [(NonEmpty Integer, Bool)] -> IO [(NonEmpty Integer, Bool)]
evalDieOp' (DieOpOptionKD kd lhw) _ is = return $ evalDieOpHelpKD kd lhw is
evalDieOp' (Reroll once o i) rng is = mapM rerollF is
  where
    rerollF g@(i', b) =
      if b && compare (NE.head i') i == o
        then do
          v <- chooseOne rerollRange
          return (v <| i', b)
        else return g
    rerollRange = if not once then filter (\i' -> not $ o == compare i' i) rng else rng

separateKeptDropped :: [(NonEmpty Integer, Bool)] -> ([(NonEmpty Integer, Bool)], [(NonEmpty Integer, Bool)])
separateKeptDropped = foldr f ([], [])
  where
    f a@(_, True) (kept, dropped) = (a : kept, dropped)
    f a@(_, False) (kept, dropped) = (kept, a : dropped)

setToDropped :: [(NonEmpty Integer, Bool)] -> [(NonEmpty Integer, Bool)]
setToDropped = fmap (\(is, _) -> (is, False))

-- TODO: make the keep/drop on low/high not require a sort somehow, or if it does to not change the output order of the values
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
instance IOEval Expr where
  evalShow (NoExpr t) = evalShow t
  evalShow (Add t e) = do
    (t', t's) <- evalShow t
    (e', e's) <- evalShow e
    return (t' + e', t's ++ " + " ++ e's)
  evalShow (Sub t e) = do
    (t', t's) <- evalShow t
    (e', e's) <- evalShow e
    return (t' - e', t's ++ " - " ++ e's)

instance IOEval Term where
  evalShow (NoTerm f) = evalShow f
  evalShow (Multi f t) = do
    (f', f's) <- evalShow f
    (t', t's) <- evalShow t
    return (f' * t', f's ++ " * " ++ t's)
  evalShow (Div f t) = do
    (f', f's) <- evalShow f
    (t', t's) <- evalShow t
    if t' == 0 -- TODO: check and make sure this bound check is correct
      then throwBot (EvaluationException "division by zero")
      else return (div f' t', f's ++ " / " ++ t's)

instance IOEval Func where
  evalShow (Id neg) = evalShow neg
  evalShow func = do
    let (fname, n) = functionDetails func
    (n', n's) <- evalShow n
    (_, f) <- getFunc fname
    return (f n', fname ++ " " ++ n's)

-- evalShow (Abs neg) = toOutType . abs <$> evalSum neg

instance IOEval Negation where
  evalShow (Neg expo) = do
    (expo', expo's) <- evalShow expo
    return (negate expo', "-" ++ expo's)
  evalShow (NoNeg expo) = evalShow expo

instance IOEval Expo where
  evalShow (NoExpo b) = evalShow b
  evalShow (Expo b expo) =
    if minVal expo < 0
      then throwBot (EvaluationException "the exponent is either negative or can be negative")
      else do
        (b', b's) <- evalShow b
        (expo', expo's) <- evalShow expo
        return (b' ^ expo', b's ++ " ^ " ++ expo's)

instance IOEval NumBase where
  evalShow (Paren e) = do
    (r, s) <- evalShow e
    return (r, "(" ++ s ++ ")")
  evalShow (Value i) = return (i, show i)

--- Finding the range of an expression.

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

instance Range Func where
  range' (Abs n) = S.fromList $ abs <$> range n
  range' (Id n) = range' n
  maxVal (Abs n) = max (abs $ maxVal n) (abs $ minVal n)
  maxVal (Id n) = maxVal n
  minVal (Abs n) = minimum $ abs <$> range n
  -- minVal (Abs n) = abs $ minVal n
  -- TODO: finish Abs min val.
  minVal (Id n) = minVal n

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
  range' (DieOpBase dop) = range' dop
  maxVal (NBase nb) = maxVal nb
  maxVal (DieOpBase dop) = maxVal dop
  minVal (NBase nb) = minVal nb
  minVal (DieOpBase dop) = minVal dop

instance Range Die where
  range' (CustomDie is) = S.fromList is
  range' (Die b) = S.fromList [1 .. (maxVal b)]
  maxVal (CustomDie is) = maximum is
  maxVal (Die b) = maxVal b
  minVal (CustomDie is) = minimum is
  minVal (Die _) = 1

instance Range MultiDie where
  range' (SingleDie d) = range' d
  range' (MultiDie nb d) = S.fromList $ do
    i <- range nb
    foldr (\a b -> ((+) <$> a) <*> b) [0] $ genericReplicate i (range d)

  maxVal (SingleDie d) = maxVal d
  maxVal (MultiDie nb d) = maxVal nb * maxVal d
  minVal (SingleDie d) = maxVal d
  minVal (MultiDie nb d) = max 0 (minVal nb) * minVal d

-- TODO: check this more
instance Range DieOp where
  range' (DieOpMulti md) = range' md
  range' d = S.unions $ fmap foldF counts
    where
      (counts, dr) = dieOpVals d
      foldF' i js
        | i < 1 = []
        | i == 1 = js
        | otherwise = ((+) <$> dr) <*> foldF' (i - 1) js
      foldF i = S.fromList $ foldF' i dr

  maxVal (DieOpMulti md) = maxVal md
  maxVal d
    | mxdr < 0 = fromMaybe 0 (minimumMay counts) * mxdr
    | otherwise = fromMaybe 0 (maximumMay counts) * mxdr
    where
      (counts, dr) = dieOpVals d
      mxdr = fromMaybe 0 $ maximumMay dr

  minVal (DieOpMulti md) = minVal md
  minVal d
    | mndr < 0 = fromMaybe 0 (maximumMay counts) * mndr
    | otherwise = fromMaybe 0 (minimumMay counts) * mndr
    where
      (counts, dr) = dieOpVals d
      mndr = fromMaybe 0 $ minimumMay dr

type DieRange = [Integer]

applyDieOpVal :: DieOpOption -> ([Integer], DieRange, DieRange) -> ([Integer], DieRange, DieRange)
applyDieOpVal (Reroll True c l) t@(is, cdr, dr)
  | any boolF cdr = (is, dr, dr)
  | otherwise = t
  where
    boolF i' = compare i' l == c
applyDieOpVal (Reroll False c l) t@(is, cdr, dr)
  | any boolF cdr = (is, filter (not . boolF) dr, dr)
  | otherwise = t
  where
    boolF i' = compare i' l == c
applyDieOpVal (DieOpOptionKD _ (Where o i)) (is, cdr, dr)
  | any boolF cdr = ([0 .. maximum is], filter (not . boolF) cdr, dr)
  | otherwise = (is, cdr, dr)
  where
    boolF i' = compare i' i == o
applyDieOpVal (DieOpOptionKD Keep lh) (is, cdr, dr) = (fmap (f (getValueLowHigh lh)) is, cdr, dr)
  where
    f (Just i) i' = min i i'
    f Nothing i' = i'
applyDieOpVal (DieOpOptionKD Drop lh) (is, cdr, dr) = (fmap (f (getValueLowHigh lh)) is, cdr, dr)
  where
    f (Just i) i' = max 0 (i' - i)
    f Nothing i' = i'

dieOpVals :: DieOp -> ([Integer], DieRange)
dieOpVals dop = (filter (>= 0) counts, dr)
  where
    (counts, dr, _) = dieOpVals' dop

dieOpVals' :: DieOp -> ([Integer], DieRange, DieRange)
dieOpVals' (DieOpMulti md) = (numberOfDiceRange, dieVals, range (getDie md))
  where
    dieVals = range (getDie md)
    numberOfDiceRange = range (getNumberOfDice md)
dieOpVals' (DieOp dop doo) = applyDieOpVal doo (dieOpVals' dop)

--- Pretty printing the AST
-- The output from this should be parseable

class PrettyShow a where
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
  prettyShow (Id n) = prettyShow n
  prettyShow f = s <> " " <> prettyShow n
    where
      (s, n) = functionDetails f

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
  prettyShow (DieOpBase dop) = prettyShow dop

instance PrettyShow Die where
  prettyShow (Die b) = "d" <> prettyShow b
  prettyShow (CustomDie is) = "d{" <> (init . tail . fromString . show) is <> "}"

instance PrettyShow MultiDie where
  prettyShow (SingleDie d) = prettyShow d
  prettyShow (MultiDie nb d) = prettyShow nb <> prettyShow d

instance PrettyShow DieOp where
  prettyShow (DieOpMulti md) = prettyShow md
  prettyShow (DieOp dop dopo) = prettyShow dop <> helper dopo
    where
      fromOrdering LT i = "<" <> fromString (show i)
      fromOrdering EQ i = "=" <> fromString (show i)
      fromOrdering GT i = ">" <> fromString (show i)
      fromLHW (Where o i) = "w" <> fromOrdering o i
      fromLHW (Low i) = "l" <> fromString (show i)
      fromLHW (High i) = "h" <> fromString (show i)
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
      matchFuncName Nothing t = return $ Id t
      matchFuncName (Just s) t
        | unpack s `member` supportedFunctions = (return . fst (supportedFunctions M.! unpack s)) t
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
    try (DieOpBase <$> pars)
      <|> try (NBase <$> pars)
      <|> fail "Could not match a base token"

instance CanParse Die where
  pars = do
    _ <- char 'd'
    try (Die <$> pars)
      <|> CustomDie <$> (try (char '{' *> skipSpace) *> (posInteger >>= (\i -> (i :) <$> many (try (skipSpace *> char ',' *> skipSpace) *> posInteger))) <* skipSpace <* char '}')
      <|> fail "recursed to die expression and could not find a die"

instance CanParse MultiDie where
  pars = do
    t <- optional $ try pars
    bd <- pars
    return $ maybe (SingleDie bd) (`MultiDie` bd) t

instance CanParse DieOp where
  pars = do
    dop <- DieOpMulti <$> pars
    parseDieOpHelp dop

parseOrdering :: Parser Ordering
parseOrdering = (char '<' <|> char '=' <|> char '>') >>= matchO
  where
    matchO '<' = return LT
    matchO '=' = return EQ
    matchO '>' = return GT
    matchO _ = fail "tried to get an ordering that didn't exist"

parseLowHigh :: Parser LowHighWhere
parseLowHigh = (char 'h' <|> char 'l' <|> char 'w') >>= helper
  where
    helper 'h' = High <$> posInteger
    helper 'l' = Low <$> posInteger
    helper 'w' = parseOrdering >>= \o -> posInteger <&> Where o
    helper _ = fail "could not determine whether to keep/drop highest/lowest"

parseDieOpOption :: Parser DieOpOption
parseDieOpOption = do
  (try (string "ro") *> parseOrdering >>= \o -> Reroll True o <$> posInteger)
    <|> (try (string "rr") *> parseOrdering >>= \o -> Reroll False o <$> posInteger)
    <|> ((try (char 'k') *> parseLowHigh) <&> DieOpOptionKD Keep)
    <|> ((try (char 'd') *> parseLowHigh) <&> DieOpOptionKD Drop)
    <|> fail "could not parse dieOpOption"

parseDieOpHelp :: DieOp -> Parser DieOp
parseDieOpHelp dop = do
  (try parseDieOpOption >>= parseDieOpHelp . DieOp dop)
    <|> return dop
