{-# LANGUAGE BangPatterns #-}

module Tablebot (
    runTablebot
) where

import Tablebot.Handler
import Tablebot.Plugin (Plugin, combinePlugins, migrations)

import Data.Text
import Discord
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Persist.Sqlite 
import Control.Monad.Logger

runTablebot :: Text -> Text -> FilePath -> [Plugin] -> IO ()
runTablebot dToken prefix dbpath plugins =
    let !plugin = combinePlugins plugins
    in do
    pool <- runNoLoggingT $ createSqlitePool (pack dbpath) 8
    -- TODO: this might have issues with duplicates?
    mapM_ (\migration -> runSqlPool (runMigration migration) pool) $ migrations plugin
    userFacingError <- runDiscord $ def {
        discordToken = dToken,
        discordOnEvent =
            flip runSqlPool pool . eventHandler plugin prefix
    }
    TIO.putStrLn userFacingError