-- |
-- Module      : Tablebot.Plugins.Say
-- Description : A command that outputs its input.
-- Copyright   : (c) Amelie WD 2021
-- License     : MIT
-- Maintainer  : tablebot@ameliewd.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs its input.
module Tablebot.Plugins.Say (sayPlugin) where

import Data.Text (Text, pack)
import Discord.Types
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser (untilEnd)
import Text.Megaparsec
import Text.RawString.QQ

-- | @say@ outputs its input.
say :: Command
say = Command "say" saycomm
  where
    saycomm :: Parser (Message -> DatabaseDiscord ())
    saycomm = do
      input <- untilEnd
      return $ \m -> do
        sendMessage m $ pack $ "> " ++ input ++ "\n - <@" ++ (show $ userId $ messageAuthor m) ++ ">"

sayHelp :: HelpPage
sayHelp =
  HelpPage
    "say"
    "make the bot speak"
    [r|**Say**
Repeat the input.

*Usage:* `say This text will be repeated by the bot!`|]
    []
    None

-- | @sayPlugin@ assembles the command into a plugin.
sayPlugin :: Plugin
sayPlugin = plug {commands = [say], helpPages = [sayHelp]}
