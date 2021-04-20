module Tablebot.Handler (
    eventHandler
) where

import Tablebot.Plugin
import Tablebot.Plugin.Types (DatabaseDiscord)
import Tablebot.Handler.Command

import Discord
import Discord.Types
import Data.Text (Text)
import Control.Monad (unless)

eventHandler :: Plugin -> Text -> Event -> DatabaseDiscord ()
eventHandler pl prefix = \case
        MessageCreate m -> unless (userIsBot (messageAuthor m)) $
            parseCommands (commands pl) m prefix
        _ -> pure ()