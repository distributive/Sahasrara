{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Tablebot.Plugin.DiceParsing
-- Description : Parsers for parsing dice and other expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the tools for parsing Dice. -Wno-orphans is enabled so that
-- parsing can occur here instead of in SmartCommand or DiceData.
module Tablebot.Plugin.Dice.DiceParsing where

import Data.Functor (($>), (<&>))
import Data.Map as M (findWithDefault, keys, map, (!))
import Data.Maybe (fromMaybe)
import Data.Set as S (fromList)
import Data.Text (Text, singleton, unpack)
import Tablebot.Plugin.Dice.DiceData
import Tablebot.Plugin.Dice.DiceFunctions
  ( ArgTypes (..),
    FuncInfoBase (..),
    basicFunctions,
    basicFunctionsList,
  )
import Tablebot.Plugin.Parser (integer, parseCommaSeparated, parseCommaSeparated1, skipSpace)
import Tablebot.Plugin.SmartCommand (CanParse (..))
import Tablebot.Plugin.Types (Parser)
import Text.Megaparsec (MonadParsec (try), choice, optional, (<?>), (<|>))
import Text.Megaparsec.Char (char, string)

--- Parsing expressions below this line

instance CanParse ListValues where
  pars = do
    LVList
      <$> ( try (char '{' *> skipSpace)
              *> parseCommaSeparated1 pars
              <* skipSpace
              <* char '}'
          )
      <|> try
        ( do
            nb <- pars
            _ <- char '#'
            MultipleValues nb <$> pars
        )
      <|> NoList
      <$> pars

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
  pars = do
    ( do
        funcName <- try (choice (string <$> basicFunctionsList)) <?> "could not find function"
        let fi = (basicFunctions @IO) M.! funcName
            ft = funcInfoParameters fi
        es <- string "(" *> skipSpace *> parseCommaSeparated pars <* skipSpace <* string ")"
        es' <- checkTypes es ft (unpack funcName)
        return $ Func fi es'
      )
      <|> NoFunc <$> pars
    where
      matchType (NoList _, ATInteger) = True
      matchType (LVList _, ATIntegerList) = True
      matchType (MultipleValues _ _, ATIntegerList) = True
      matchType _ = False
      checkTypes es ft fname
        | length es > length ft = fail $ "too many values given to function " ++ fname
        | length ft > length es = fail $ "too few values given to function " ++ fname
        | length matched /= length es = fail $ "type mismatch in parameters to function " ++ fname ++ ", in parameter " ++ show (length matched)
        | otherwise = return es
        where
          matched = takeWhile matchType (zip es ft)

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
    (try (skipSpace *> char '(') *> skipSpace *> (Paren . unnest <$> pars) <* skipSpace <* char ')')
      <|> try (Value <$> integer)
    where
      unnest (NoExpr (NoTerm (NoNeg (NoExpo (NoFunc (NBase (Paren e))))))) = e
      unnest e = e

instance CanParse Base where
  pars = try (DiceBase <$> pars) <|> try (NBase <$> pars)

instance CanParse Die where
  pars = do
    _ <- char 'd'
    lazyFunc <- (try (char '!') $> LazyDie) <|> return id
    try (lazyFunc . Die <$> pars)
      <|> lazyFunc . CustomDie
        <$> ( try (char '{' *> skipSpace)
                *> parseCommaSeparated1 pars
                <* skipSpace
                <* char '}'
            )

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

-- | Parse a `/=`, `<=`, `>=`, `<`, `=`, `>` as an `AdvancedOrdering`.
parseAdvancedOrdering :: Parser AdvancedOrdering
parseAdvancedOrdering = (try (choice opts) <?> "could not parse an ordering") >>= matchO
  where
    matchO :: Text -> Parser AdvancedOrdering
    matchO s = M.findWithDefault (failure' s (S.fromList opts')) s (M.map return $ fst advancedOrderingMapping)
    opts' = M.keys $ fst (advancedOrderingMapping @Text)
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
  ( (try (string "ro") *> parseAdvancedOrdering >>= \o -> Reroll True o <$> pars)
      <|> (try (string "rr") *> parseAdvancedOrdering >>= \o -> Reroll False o <$> pars)
      <|> ((try (char 'k') *> parseLowHigh) <&> DieOpOptionKD Keep)
      <|> ((try (char 'd') *> parseLowHigh) <&> DieOpOptionKD Drop) <&> lazyFunc
    )
    <?> "could not parse dieOpOption - expecting one of the options described in the doc (call `help roll` to access)"
