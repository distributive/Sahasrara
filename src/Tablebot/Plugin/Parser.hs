{-|
Module      : Tablebot.Plugin.Parser
Description : Helpful parsers for building plugins.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

This module contains helpful parsers for building plugins, along with a few
reexports from "Text.Parsec" to avoid having to import it.
-}
module Tablebot.Plugin.Parser (
    module Tablebot.Plugin.Parser,
    -- | A Parser on @Text@ inputs (reexport from "Text.Parsec.Text").
    Parser
) where

-- TODO: Much helpful functionality is missing here.

import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Functor (($>))

-- | @skipSpace@ is a parser that skips many space characters.
skipSpace :: Parser ()
skipSpace = skipMany space

-- | @noArguments@ is a parser that only accepts space characters followed by
-- an end-of-file character, and then runs the input function @f@. Useful for
-- building parsers for commands that take no arguments.
noArguments :: a -> Parser a
noArguments f = (skipSpace *> (eof <?> "No arguments were needed!")) $> f

-- | @quoted@ looks for a quoted string - i.e. one of the form @"text"@.
-- It returns the string found within the quotes if successful, or throws a
-- clear error.
quoted :: Parser String
-- TODO: deal with backslash escapes properly.
-- Also consider "" and '' quoting.
quoted = between (char '"' <?> "Couldn't find opening quote.")
    (char '"' <?> "Couldn't find closing quote.")
    (many1 $ noneOf ['"']) <?> "Couldn't get quote!"

-- | @word@ parses a single word of letters only.
word :: Parser String
word = many1 letter

-- | @number@ parses any whole, non-negative number.
number :: Parser Int
number = read <$> many1 digit

-- | @untilEnd@ gets all of the characters up to the end of the input.
untilEnd :: Parser String
untilEnd = do
    c <- anyChar
    cs <- manyTill anyChar eof
    return (c:cs)

-- | @discordUser@ gets a Discord user from its input.
-- This means that it matches @<\@longidhere>@.
discordUser :: Parser String
discordUser = do
    num <- between (string "<@") (char '>') (many1 digit)
    return $ "<@" ++ num ++ ">"

-- | @sp@ parses an optional space character.
sp :: Parser ()
sp = optional space