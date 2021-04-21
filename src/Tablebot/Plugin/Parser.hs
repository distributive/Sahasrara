module Tablebot.Plugin.Parser (
    Parser, module Tablebot.Plugin.Parser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Functor (($>))

skipSpace :: Parser ()
skipSpace = skipMany space

noArguments :: a -> Parser a
noArguments f = (skipSpace *> (eof <?> "No arguments were needed!")) $> f

-- TODO: deal with backslash escapes properly.
-- Also consider "" and '' quoting.
quoted :: Parser String
quoted = between (char '"' <?> "Couldn't find opening quote.")
    (char '"' <?> "Couldn't find closing quote.")
    (many1 $ noneOf ['"']) <?> "Couldn't get quote!"

word :: Parser String
word = many1 letter

number :: Parser Int
number = read <$> many1 digit

untilEnd :: Parser String
untilEnd = do
    c <- anyChar
    cs <- manyTill anyChar eof
    return (c:cs)

discordUser :: Parser String
discordUser = do
    num <- between (string "<@") (char '>') (many1 digit)
    return $ "<@" ++ num ++ ">"

sp :: Parser ()
sp = optional space