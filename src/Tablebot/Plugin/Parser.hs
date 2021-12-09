-- |
-- Module      : Tablebot.Plugin.Parser
-- Description : Helpful parsers for building plugins.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains helpful parsers for building plugins, along with a few
-- reexports from "Text.Parsec" to avoid having to import it.
module Tablebot.Plugin.Parser where

import Data.Char (isDigit, isLetter, isSpace)
import Data.Functor (($>))
import Tablebot.Plugin (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char, alphaNumChar)

space :: Parser ()
space = satisfy isSpace $> ()

notSpace :: Parser Char
notSpace = satisfy $ not . isSpace

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isLetter

-- | @skipSpace@ is a parser that skips many space characters.
skipSpace :: Parser ()
skipSpace = skipMany space

-- | @skipSpace1@ is a parser that skips at least one space character.
skipSpace1 :: Parser ()
skipSpace1 = skipSome space

-- | @quoted@ looks for a quoted string - i.e. one of the form @"text"@.
-- It returns the string found within the quotes if successful, or throws a
-- clear error.
quoted :: Parser String
-- TODO: deal with backslash escapes properly.
quoted = quotedWith '"' <|> quotedWith '\''
  where
    quotedWith :: Char -> Parser String
    quotedWith c =
      between
        (single c <?> "Couldn't find opening quote.")
        (single c <?> "Couldn't find closing quote.")
        (some $ anySingleBut c)
        <?> "Couldn't get quote!"

-- | @word@ parses a single word of letters only.
word :: Parser String
word = some letter

-- | @nonSpaceWord@ parses a single word of any non-space characters.
nonSpaceWord :: Parser String
nonSpaceWord = some notSpace

-- | @number@ parses any whole, non-negative number.
number :: Parser Int
number = read <$> some digit

-- | @untilEnd@ gets all of the characters up to the end of the input.
untilEnd :: Parser String
untilEnd = manyTill anySingle eof

-- | @untilEnd1@ gets all of the characters up to the end of the input,
-- requiring there to be at least one.
untilEnd1 :: Parser String
untilEnd1 = do
  c <- anySingle
  cs <- manyTill anySingle eof
  return (c : cs)

-- | @discordUser@ gets a Discord user from its input.
-- This means that it matches @<\@longidhere>@.
discordUser :: Parser String
discordUser = do
  num <- between (chunk "<@") (single '>') (some digit)
  return $ "<@" ++ num ++ ">"

-- | @netrunnerQuery@ gets an inline Netrunner search query.
-- This means that it matches @{{card title}}@.
netrunnerQuery :: Parser String
netrunnerQuery = between (chunk "{{") (chunk "}}") $ some alphaNumChar
  -- _ <- chunk "{{"
  -- card <- manyTill alphaNumChar $ chunk "}}"
  -- _ <- chunk "}}"
  -- return card

-- | @sp@ parses an optional space character.
sp :: Parser ()
sp = space <|> pure ()

-- | @posInteger@ parses an integer with no "-".
posInteger :: (Integral a, Read a) => Parser a
posInteger = do
  digits <- some digit
  return (read digits)

-- | @integer@ parses any whole number.
integer :: (Integral a, Read a) => Parser a
integer = do
  minus <- char '-' <|> return ' '
  digits <- some digit
  return (read (minus : digits))

-- | @double@ parses any decimal number.
double :: Parser Double
double = do
  minus <- char '-' <|> return ' '
  digits <- some digit
  decimal <-
    ( do
        _ <- char '.'
        num <- some digit
        return $ '.' : num
      )
      <|> return ""
  return (read (minus : digits ++ decimal))
