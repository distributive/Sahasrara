-- |
-- Module      : Tablebot.Plugins.Say
-- Description : A command that outputs its input.
-- Copyright   : (c) Amelie WD 2021
-- License     : MIT
-- Maintainer  : tablebot@ameliewd.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands that
module Tablebot.Plugins.Administration (restartPlugin) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Text (Text, pack)
import Discord (stopDiscord)
import Discord.Types
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser (untilEnd)
import Text.Megaparsec
import Text.RawString.QQ

-- | @restart@ reloads the bot with any new configuration changes.
restart :: Command
restart = Command "restart" saycomm
  where
    saycomm :: Parser (Message -> DatabaseDiscord ())
    saycomm = do
      input <- untilEnd
      return $ \m -> do
        sendMessage m "Restarting"
        lift $ stopDiscord

restartHelp :: HelpPage
restartHelp =
  HelpPage
    "restart"
    "restart the bot"
    [r|**Restart**
Restart the bot

*Usage:* `reastart`|]
    []
    None

-- | @restartPlugin@ assembles the command into a plugin.
restartPlugin :: Plugin
restartPlugin = (plug "restart") {commands = [restart], helpPages = [restartHelp]}
