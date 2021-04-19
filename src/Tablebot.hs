{-# LANGUAGE BangPatterns #-}

module Tablebot (
    runTablebot
) where

import Tablebot.Handler
import Tablebot.Plugin (Plugin, combinePlugins)

import Data.Text
import Discord
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Selda.SQLite

-- TODO (very far in future): choose between SQLite and Postgres.
runTablebot :: Text -> Text -> FilePath -> [Plugin SQLite] -> IO ()
runTablebot dToken prefix dbpath plugins =
    let !plugin = combinePlugins plugins
    in do
    userFacingError <- runDiscord $ def {
        discordToken = dToken,
        discordOnEvent =
            withSQLite dbpath . eventHandler plugin prefix
    }
    TIO.putStrLn userFacingError