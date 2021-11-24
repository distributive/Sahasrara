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
module Tablebot.Plugin.Dice where

import Data.Functor ((<&>))
import Data.List (genericDrop, genericReplicate, genericTake, sortOn)
import Data.Map.Strict as M (Map, fromList, member, (!))
import Data.Set as S (Set, fromList, singleton, toList)
import Data.Text (pack, unpack)
import System.Random (Random (randomRIO))
import Tablebot.Plugin.Exception (BotException (EvaluationException), throwBot)
import Tablebot.Plugin.Parser (posInteger, skipSpace, word)
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

-- TODO: make this do something

-- | The maximum depth that should be permitted. Used to limit number of dice and rerolls.
maximumRecursion :: Integer
maximumRecursion = 100

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

data KeepDrop = Keep | Drop deriving (Show, Eq)

-- Mappings for what functions are supported

supportedFunctions :: Map String (Negation -> Func, Integer -> Integer)
supportedFunctions =
  M.fromList
    [ ("abs", (Abs, abs)),
      ("id", (Id, id))
    ]

functionDetails :: Func -> (String, Negation)
functionDetails (Id a) = ("id", a)
functionDetails (Abs a) = ("abs", a)

--- Evaluating an expression. Uses IO because dice are random

evalExpr :: Expr -> IO Integer
evalExpr a = sumDiceVals <$> eval a

sumDiceVals :: [(Integer, a)] -> Integer
sumDiceVals = foldr (\(i, _) b -> i + b) 0

toOutType :: Integer -> [(Integer, Die)]
toOutType i = [(i, CustomDie [])]

-- TODO: make eval's type evalSum's type, and add a function that builds an output string
--  representing the change
class IOEval a where
  eval :: a -> IO [(Integer, Die)]
  evalSum :: a -> IO Integer
  evalSum a = sumDiceVals <$> eval a

instance IOEval Base where
  eval (NBase nb) = eval nb
  eval (DieOpBase dop) = do
    is <- eval dop
    return [(sumDiceVals is, CustomDie [])]

instance IOEval Die where
  eval d@(CustomDie is) = do
    i <- chooseOne is
    return [(i, d)]
  eval d@(Die b) = do
    upper <- evalSum b
    if upper < 1
      then throwBot $ EvaluationException "Cannot roll a < 1 sided die"
      else do
        i <- randomRIO (1, upper)
        return [(i, d)]

instance IOEval MultiDie where
  eval (SingleDie d) = eval d
  eval (MultiDie nb d) = do
    i <- evalSum nb
    if i < 0
      then throwBot (EvaluationException "tried to give a negative value to the number of dice")
      else do
        ds <- sequence $ genericReplicate i $ eval d
        return $ concat ds

instance IOEval DieOp where
  eval (DieOpMulti md) = eval md
  eval (DieOp dop dopo) = do
    dop' <- eval dop
    evalDieOp dopo dop'

evalDieOp :: DieOpOption -> [(Integer, Die)] -> IO [(Integer, Die)]
evalDieOp (DieOpOptionKD kd lhw) is = return (evalDieOpHelpKD kd lhw is)
evalDieOp r@(Reroll once o i) is = do
  if once
    then mapM rerollOnceF is
    else do
      let dierange = fmap (\(i', d) -> (i', d, filter (\i'' -> compare i'' i /= o) (range d))) is
      if any (\(_, _, x) -> null x) dierange
        then throwBot (EvaluationException $ "Infinite reroll request out of bounds (" ++ show r ++ ")")
        else mapM (\(i', d, lst) -> if compare i' i == o then chooseOne lst >>= \i'' -> return (i'', d) else return (i', d)) dierange
  where
    rerollOnceF g@(i', d) =
      if compare i' i == o
        then do
          val <- eval d
          if null val
            then fail "could not get die value (reroll)"
            else return $ head val
        else return g

evalDieOpHelpKD :: KeepDrop -> LowHighWhere -> [(Integer, Die)] -> [(Integer, Die)]
evalDieOpHelpKD Keep (Where cmp i) = filter (\(i', _) -> compare i' i == cmp)
evalDieOpHelpKD Drop (Where cmp i) = filter (\(i', _) -> compare i' i /= cmp)
evalDieOpHelpKD Keep (Low i) = genericTake i . sortOn fst
evalDieOpHelpKD Drop (Low i) = genericDrop i . sortOn fst
evalDieOpHelpKD Keep (High i) = genericTake i . sortOn (negate . fst)
evalDieOpHelpKD Drop (High i) = genericDrop i . sortOn (negate . fst)

--- Pure evaluation functions for non-dice calculations
-- Was previously its own type class that wouldn't work for evaluating Base values.
instance IOEval Expr where
  eval (NoExpr t) = eval t
  eval (Add t e) = toOutType <$> (((+) <$> evalSum t) <*> evalSum e)
  eval (Sub t e) = toOutType <$> (((-) <$> evalSum t) <*> evalSum e)

instance IOEval Term where
  eval (NoTerm f) = eval f
  eval (Multi f t) = toOutType <$> (((*) <$> evalSum f) <*> evalSum t)
  eval (Div f t) = do
    f' <- evalSum f
    t' <- evalSum t
    if t' == 0
      then throwBot (EvaluationException "division by zero")
      else return $ toOutType $ div f' t'

instance IOEval Func where
  eval (Id neg) = eval neg
  eval (Abs neg) = toOutType . abs <$> evalSum neg

instance IOEval Negation where
  eval (Neg expo) = toOutType . negate <$> evalSum expo
  eval (NoNeg expo) = eval expo

instance IOEval Expo where
  eval (NoExpo b) = eval b
  eval (Expo b expo) = do
    b' <- evalSum b
    expo' <- evalSum expo
    if expo' < 0
      then throwBot (EvaluationException "negative exponent")
      else return $ toOutType $ b' ^ expo'

instance IOEval NumBase where
  eval (Paren e) = eval e
  eval (Value i) = return $ toOutType i

--- Finding the range of an expression.
-- TODO: `DieOp` still needs to be completed
-- it would also be good to have custom maxvals and ranges for everything

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
  minVal (Die b) = minVal b

instance Range MultiDie where
  range' (SingleDie d) = range' d
  range' (MultiDie nb d) = S.fromList $ do
    i <- [nbMin' .. nbMax]
    foldr (\a b -> ((+) <$> a) <*> b) [0] $ genericReplicate i (range d)
    where
      nbRange = range nb
      nbMin = minimum nbRange
      nbMax = maximum nbRange
      nbMin' = max 0 nbMin

  maxVal (SingleDie d) = maxVal d
  maxVal (MultiDie nb d) = maxVal nb * maxVal d
  minVal (SingleDie d) = maxVal d
  minVal (MultiDie nb d) = minVal nb * minVal d

-- TODO: this
instance Range DieOp where
  range' (DieOpMulti md) = range' md

--- Pretty printing the AST
-- The output from this should be parseable

class PrettyShow a where
  prettyShow :: a -> String

instance PrettyShow Expr where
  prettyShow (Add t e) = prettyShow t <> " + " <> prettyShow e
  prettyShow (Sub t e) = prettyShow t <> " + " <> prettyShow e
  prettyShow (NoExpr t) = prettyShow t

instance PrettyShow Term where
  prettyShow (Multi f t) = prettyShow f <> " * " <> prettyShow t
  prettyShow (Div f t) = prettyShow f <> " / " <> prettyShow t
  prettyShow (NoTerm f) = prettyShow f

instance PrettyShow Func where
  prettyShow f = s <> " " <> prettyShow n
    where
      (s, n) = functionDetails f

instance PrettyShow Negation where
  prettyShow (Neg expo) = "-" <> prettyShow expo
  prettyShow (NoNeg expo) = prettyShow expo

instance PrettyShow Expo where
  prettyShow (NoExpo b) = prettyShow b
  prettyShow (Expo b expo) = prettyShow b <> "^" <> prettyShow expo

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
    funcName <- optional $ try (pack <$> word) <* skipSpace
    t <- pars
    matchFuncName funcName t
    where
      matchFuncName Nothing t = return $ Id t
      matchFuncName (Just s) t
        | unpack s `member` supportedFunctions = (return . fst (supportedFunctions M.! unpack s)) t
        | otherwise = fail $ "Could not find function with name" ++ unpack s

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
