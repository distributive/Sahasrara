{-# LANGUAGE BangPatterns #-}

module Tablebot (
    runTablebot
) where

import Tablebot.Handler
import Tablebot.Plugin (Plugin, combinePlugins, migrations, cronJobs)

import Data.Text
import Discord
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Persist.Sqlite 
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Control.Monad.IO.Class
import Control.Concurrent

runTablebot :: Text -> Text -> FilePath -> [Plugin] -> IO ()
runTablebot dToken prefix dbpath plugins =
    let !plugin = combinePlugins plugins
    in do
    -- Create multiple database threads.
    pool <- runNoLoggingT $ createSqlitePool (pack dbpath) 8
    -- TODO: this might have issues with duplicates?
    mapM_ (\migration -> runSqlPool (runMigration migration) pool) $ migrations plugin
    -- Create a var to kill any ongoing tasks.
    mvar <- newEmptyMVar :: IO (MVar [ThreadId])
    userFacingError <- runDiscord $ def {
        discordToken = dToken,
        discordOnEvent =
            flip runSqlPool pool . eventHandler plugin prefix,
        discordOnStart =
            -- Build list of cron jobs.
            runSqlPool (mapM runCron (cronJobs plugin) >>= liftIO . putMVar mvar) pool,
        -- Kill every cron job provided.
        discordOnEnd = takeMVar mvar >>= killCron
    }
    TIO.putStrLn userFacingError