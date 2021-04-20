module Tablebot.Plugin.Parser (
    noArguments, skipSpace, Parser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Functor (($>))

skipSpace :: Parser ()
skipSpace = skipMany space

noArguments :: a -> Parser a
noArguments f = (skipSpace *> (eof <?> "No arguments were needed!")) $> f