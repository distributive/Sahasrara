{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module Plugin.Plugin (
    Plugin, Feature(..), eventHandler
) where

import Plugin.Types
import Plugin.Command

import Discord
import Discord.Types
import Data.Text (Text)
import Database.Redis (Connection)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

-- TODO: short rewrite with withReaderT, where r' = DiscordHandlerEnv, r = FeatureEnv
-- TODO: parser is broken. fix
eventHandler :: [Plugin] -> Connection -> Text -> Event -> DiscordHandler ()
eventHandler pls rconn prefix =
    let !commandFns = toCommandFns pls
    in \event -> do -- DiscordHandler ()
        discord <- ask
        let env = FEnv {rconn, discord}
        lift $ flip runReaderT env $ case event of
            MessageCreate m -> doCommand prefix m $! toCommandFns pls
            _ -> pure ()