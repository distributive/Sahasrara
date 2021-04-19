module Tablebot.Handler (
    eventHandler
) where

import Tablebot.Plugin
import Tablebot.Handler.Command

import Discord
import Discord.Types
import Data.Text (Text)
import Database.Selda
import Control.Monad (unless)

eventHandler :: Plugin b -> Text -> Event -> SeldaT b DiscordHandler ()
eventHandler pl prefix = \case
        MessageCreate m -> unless (userIsBot (messageAuthor m)) $
            doCommand prefix m (commands pl)
        _ -> pure ()