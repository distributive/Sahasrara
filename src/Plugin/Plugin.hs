{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module Plugin.Plugin (
    Plugin, Feature(..), eventHandler
) where

import Plugin.Types
import Plugin.Command

import Discord
import Discord.Types
import Data.Text (Text)
import Database.Selda
import Control.Monad (unless)

-- TODO: parser is broken. fix
eventHandler :: [Plugin b] -> Text -> Event -> SeldaT b DiscordHandler ()
eventHandler pls prefix =
    let !commandFns = toCommandFns pls
    in \case
        MessageCreate m -> unless (userIsBot (messageAuthor m)) $
            doCommand prefix m commandFns
        _ -> pure ()