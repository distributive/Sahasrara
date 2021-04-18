{-# LANGUAGE NamedFieldPuns #-}

module Plugin.Plugin (
    Plugin, Feature
) where

import Plugin.Types
import Plugin.Command

import Discord
import Discord.Types
import Data.Text (Text)
import Database.Redis (Connection)

eventHandler :: [Plugin] -> Connection -> DiscordHandle -> Text -> Event -> IO ()
eventHandler pls rconn discord prefix = let env = FEnv {rconn, discord} in
    \event -> case event of
        MessageCreate m -> doCommand prefix m $! toCommandFns pls -- TODO: match on Command, map to its inner parser, run, apply m
