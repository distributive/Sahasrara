-- |
-- Module      : Tablebot.Utility.Parser
-- Description : Helpful parsers for building plugins.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains helpful parsers for building plugins, along with a few
-- reexports from "Text.Parsec" to avoid having to import it.
module Tablebot.Utility.Parser where

import Data.Char (isDigit, isLetter, isSpace)
import Data.Functor (void, ($>))
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Internal.Rest (Message)
import Tablebot.Utility
import Tablebot.Utility.Discord (reactToMessage)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)

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
quoted = quotedWith '"' <|> quotedWith '\'' <|> specialQuotes
  where
    quotedWith :: Char -> Parser String
    quotedWith c =
      between
        (single c <?> "Couldn't find opening quote.")
        (single c <?> "Couldn't find closing quote.")
        (some $ anySingleBut c)
        <?> "Couldn't get quote!"
    specialQuotes :: Parser String
    specialQuotes = do
      let open = '“'
          closed = '”'
      _ <- single open
      q <- some $ anySingleBut closed
      _ <- single closed
      return q

quotedWithout :: [Char] -> Parser String
quotedWithout excl = quotedWith '"' <|> quotedWith '\''
  where
    quotedWith :: Char -> Parser String
    quotedWith c =
      between
        (single c <?> "Couldn't find opening quote.")
        (single c <?> "Couldn't find closing quote.")
        (some $ satisfy (`notElem` c : excl))
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

-- | @keyValue@ gets a set of key/value pairs where keys are separated from
-- values by colons. Invalid strings between and surrounding pairs are ignored.
-- It matches @a:value b:"val ue" c:value ...@
keyValue :: Parser [(String, String)]
keyValue = many $ try $ skipManyTill anySingle pair
  where
    pair :: Parser (String, String)
    pair = do
      cat <- word
      _ <- ":"
      content <- quoted <|> nonSpaceWord
      return (cat, content)

-- | @keyValueSepOn@ is @keyValue@ except is allows a given list of key/value
-- separators.
keyValueSepOn :: [Char] -> Parser [(String, Char, String)]
keyValueSepOn seps = many $ try $ skipManyTill anySingle pair
  where
    pair :: Parser (String, Char, String)
    pair = do
      cat <- word
      sep <- satisfy (`elem` seps)
      content <- quoted <|> nonSpaceWord
      return (cat, sep, content)

-- | @keyValuesSepOn@ is @keyValue@ except is allows a given list of key/value
-- separators, and a list of `or` dividers.
keyValuesSepOn :: [Char] -> [Char] -> Parser [(String, Char, [String])]
keyValuesSepOn seps ors = many $ try $ skipManyTill anySingle pair
  where
    pair :: Parser (String, Char, [String])
    pair = do
      cat <- word
      sep <- satisfy (`elem` seps)
      content <- (quotedWithout ors <|> nonSpaceWord') `sepBy` satisfy (`elem` ors)
      return (cat, sep, content)
    nonSpaceWord' :: Parser String
    nonSpaceWord' = some $ satisfy $ \c -> not (isSpace c) && (c `notElem` ors)

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

-- | For helping to create inline commands. Takes the opening characters, closing
-- characters, a parser to get a value `e`, and an action that takes that `e` and a
-- message and produces a DatabaseDiscord effect.
inlineCommandHelper :: Text -> Text -> Parser e -> (e -> Message -> EnvDatabaseDiscord d ()) -> EnvInlineCommand d
inlineCommandHelper open close p action =
  InlineCommand
    ( do
        getExprs <- some (try $ skipManyTill anySingle (string open *> skipSpace *> (((Right <$> try p) <* skipSpace <* string close) <|> (Left . T.pack <$> manyTill anySingle (string close)))))
        return $ \m -> mapM_ (`action'` m) (take maxInlineCommands getExprs)
    )
  where
    maxInlineCommands = 3
    action' (Right p') m = action p' m
    action' (Left _) m = void $ reactToMessage m "x"

-- | Parse 0 or more comma separated values.
parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p = do
  f <- optional $ try p
  maybe (return []) (\first' -> (first' :) <$> many (try (skipSpace *> char ',' *> skipSpace) *> p)) f

-- | Parse 1 or more comma separated values.
parseCommaSeparated1 :: Parser a -> Parser [a]
parseCommaSeparated1 p = do
  p >>= (\first' -> (first' :) <$> many (try (skipSpace *> char ',' *> skipSpace) *> p))
