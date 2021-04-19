module Tablebot.Plugin.Parser (
    noArguments
) where

import Tablebot.Plugin (SeldaDiscord)

import Discord.Types
import Data.Attoparsec.Text
import Data.Functor (($>))

noArguments :: (Message -> SeldaDiscord b ()) -> Parser (Message -> SeldaDiscord b ())
noArguments f = (skipSpace *> endOfInput <?> "No arguments were needed!") $> f