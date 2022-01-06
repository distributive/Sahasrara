{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceParsing
-- Description : Parsers for parsing dice and other expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the tools for parsing Dice. -Wno-orphans is enabled so
-- that parsing can occur here instead of in SmartParser or DiceData.
module Tablebot.Plugins.Roll.Dice.DiceParsing () where

import Data.Functor (($>), (<&>))
import Data.List.NonEmpty as NE (fromList)
import Data.Map as M (Map, findWithDefault, keys, map, (!))
import Data.Maybe (fromMaybe)
import Data.Set as S (Set, fromList, map)
import Data.Text (Text, singleton, unpack)
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceFunctions
  ( ArgType (..),
    FuncInfoBase (..),
    integerFunctions,
    listFunctions,
  )
import Tablebot.Utility.Parser (integer, parseCommaSeparated1, skipSpace)
import Tablebot.Utility.SmartParser (CanParse (..))
import Tablebot.Utility.Types (Parser)
import Text.Megaparsec (MonadParsec (try), choice, failure, optional, (<?>), (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (ErrorItem (Tokens))

-- | An easier way to handle failure in parsers.
failure' :: Text -> Set Text -> Parser a
failure' s ss = failure (Just $ Tokens $ NE.fromList $ unpack s) (S.map (Tokens . NE.fromList . unpack) ss)

instance CanParse ListValues where
  pars =
    do
      try (LVBase <$> pars)
      <|> try
        ( do
            nb <- pars
            _ <- char '#'
            MultipleValues nb <$> pars
        )
      <|> functionParser (listFunctions @IO) LVFunc

instance CanParse ListValuesBase where
  pars = do
    LVBList
      <$> ( try (char '{' *> skipSpace)
              *> parseCommaSeparated1 pars
              <* skipSpace
              <* char '}'
          )
      <|> LVBParen
      <$> pars

-- | Helper function to try to parse the second part of a binary operator.
binOpParseHelp :: (CanParse a) => Char -> (a -> a) -> Parser a
binOpParseHelp c con = try (skipSpace *> char c) *> skipSpace *> (con <$> pars)

instance CanParse Expr where
  pars = do
    t <- pars
    binOpParseHelp '+' (Add t) <|> binOpParseHelp '-' (Sub t) <|> (return . NoExpr) t

instance CanParse Term where
  pars = do
    t <- pars
    binOpParseHelp '*' (Multi t) <|> binOpParseHelp '/' (Div t) <|> (return . NoTerm) t

instance CanParse Func where
  pars = try (functionParser (integerFunctions @IO) Func) <|> NoFunc <$> pars

-- | A generic function parser that takes a mapping from function names to
-- functions, the main way to contruct the function data type `e`, and a
-- constructor for `e` that takes only one value, `a` (which has its own,
-- previously defined parser).
functionParser :: M.Map Text (FuncInfoBase m j) -> (FuncInfoBase m j -> [ArgValue] -> e) -> Parser e
functionParser m mainCons =
  do
    fi <- try (choice (string <$> M.keys m) >>= \t -> return (m M.! t)) <?> "could not find function"
    let ft = funcInfoParameters fi
    es <- skipSpace *> string "(" *> skipSpace *> parseArgValues ft <* skipSpace <* (try (string ")") <?> "expected only " ++ show (length ft) ++ " arguments, got more")
    return $ mainCons fi es

instance CanParse Negation where
  pars =
    try (char '-') *> skipSpace *> (Neg <$> pars)
      <|> NoNeg <$> pars

instance CanParse Expo where
  pars = do
    t <- pars
    binOpParseHelp '^' (Expo t) <|> (return . NoExpo) t

instance CanParse NumBase where
  pars =
    try (NBParen . unnest <$> pars)
      <|> Value <$> integer
    where
      unnest (Paren (NoExpr (NoTerm (NoNeg (NoExpo (NoFunc (NBase (NBParen (Paren e))))))))) = Paren e
      unnest e = e

instance (CanParse a) => CanParse (Paren a) where
  pars = char '(' *> skipSpace *> (Paren <$> pars) <* skipSpace <* char ')'

instance CanParse Base where
  pars = try (DiceBase <$> pars) <|> try (NBase <$> pars)

instance CanParse Die where
  pars = do
    _ <- char 'd'
    lazyFunc <- (try (char '!') $> LazyDie) <|> return id
    try
      ( lazyFunc . CustomDie
          <$> pars
      )
      <|> lazyFunc . Die
      <$> pars

instance CanParse Dice where
  pars = do
    t <- optional $ try (pars :: Parser NumBase)
    bd <- parseDice'
    let t' = NBase $ fromMaybe (Value 1) t
    return $ bd t'

-- | Helper for parsing Dice, where as many `Dice` as possible are parsed and a
-- function that takes a `Base` value and returns a `Dice` value is returned.
-- This `Base` value is meant to be first value that `Dice` have.
parseDice' :: Parser (Base -> Dice)
parseDice' = do
  d <- pars :: Parser Die
  mdor <- parseDieOpRecur
  ( do
      bd <- try parseDice'
      return (\b -> bd (DiceBase $ Dice b d mdor))
    )
    <|> return (\b -> Dice b d mdor)

-- | Parse a `/=`, `<=`, `>=`, `<`, `=`, `>` as an `AdvancedOrdering`.
parseAdvancedOrdering :: Parser AdvancedOrdering
parseAdvancedOrdering = (try (choice opts) <?> "could not parse an ordering") >>= matchO
  where
    matchO :: Text -> Parser AdvancedOrdering
    matchO s = M.findWithDefault (failure' s (S.fromList opts')) s (M.map return $ fst advancedOrderingMapping)
    opts' = M.keys $ fst advancedOrderingMapping
    opts = fmap string opts'

-- | Parse a `LowHighWhere`, which is an `h` followed by an integer.
parseLowHigh :: Parser LowHighWhere
parseLowHigh = (try (choice @[] $ char <$> "lhw") <?> "could not parse high, low or where") >>= helper
  where
    helper 'h' = High <$> pars
    helper 'l' = Low <$> pars
    helper 'w' = parseAdvancedOrdering >>= \o -> pars <&> Where o
    helper c = failure' (singleton c) (S.fromList ["h", "l", "w"])

-- | Parse a bunch of die options into, possibly, a DieOpRecur.
parseDieOpRecur :: Parser (Maybe DieOpRecur)
parseDieOpRecur = do
  dopo <- optional (try parseDieOpOption)
  maybe (return Nothing) (\dopo' -> Just . DieOpRecur dopo' <$> parseDieOpRecur) dopo

-- | Parse a single die option.
parseDieOpOption :: Parser DieOpOption
parseDieOpOption = do
  lazyFunc <- (try (char '!') $> DieOpOptionLazy) <|> return id
  ( ( (try (string "ro") *> parseAdvancedOrdering >>= \o -> Reroll True o <$> pars)
        <|> (try (string "rr") *> parseAdvancedOrdering >>= \o -> Reroll False o <$> pars)
        <|> ((try (char 'k') *> parseLowHigh) <&> DieOpOptionKD Keep)
        <|> ((try (char 'd') *> parseLowHigh) <&> DieOpOptionKD Drop)
    )
      <&> lazyFunc
    )
    <?> "could not parse dieOpOption - expecting one of the options described in the doc (call `help roll` to access)"

parseArgValue :: ArgType -> Parser ArgValue
parseArgValue ATIntegerList = AVListValues <$> try pars <?> "could not parse a list value from the argument"
parseArgValue ATInteger = AVExpr <$> try pars <?> "could not parse an integer from the argument"

parseArgValues :: [ArgType] -> Parser [ArgValue]
parseArgValues [] = return []
parseArgValues [at] = (: []) <$> parseArgValue at
parseArgValues (at : ats) = parseArgValue at >>= \av -> skipSpace *> (try (char ',') <?> "expected " ++ show (length ats) ++ " more arguments") *> skipSpace *> ((av :) <$> parseArgValues ats)
