{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Tablebot
-- Description : The main runner for the Tablebot Discord bot.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the main runner for Tablebot. If you're just looking to
-- run the bot with existing plugins, importing this and your favourite plugins
-- from "Tablebot.Plugins".
module Tablebot
  ( runTablebot,
  )
where

import Control.Concurrent
  ( MVar,
    ThreadId,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Persist.Sqlite
  ( createSqlitePool,
    runMigration,
    runSqlPool,
  )
import Discord
import Tablebot.Handler (eventHandler, killCron, runCron)
import Tablebot.Handler.Administration (adminMigration, currentBlacklist, removeBlacklisted)
import Tablebot.Plugin (Plugin, combinePlugins, cronJobs, migrations)
import Tablebot.Plugin.Help
import Tablebot.Plugin.Utils (debugPrint)

-- | runTablebot @dToken@ @prefix@ @dbpath@ @plugins@ runs the bot using the
-- given Discord API token @dToken@ and SQLite connection string @dbpath@. Only
-- the plugins provided by @plugins@ are run, and all commands are prefixed
-- with @prefix@.
-- The plugins given are combined into a single plugin with their combined
-- functionality. Each migration present in the combined plugin is run, and
-- each cron job and handler is set up.
-- This creates a small pool of database connections used by the event handler,
-- builds an event handler and starts cron jobs. It also kills the cron jobs on
-- bot close.
runTablebot :: Text -> Text -> FilePath -> [Plugin] -> IO ()
runTablebot dToken prefix dbpath plugins =
  do
    debugPrint ("DEBUG enabled. This is strongly not recommended in production!" :: String)
    -- Create multiple database threads.
    pool <- runNoLoggingT $ createSqlitePool (pack dbpath) 8

    -- Setup and then apply plugin blacklist from the database
    runSqlPool (runMigration adminMigration) pool
    blacklist <- runResourceT $ runNoLoggingT $ runSqlPool currentBlacklist pool
    let !plugin = generateHelp $ combinePlugins (removeBlacklisted blacklist plugins)

    -- TODO: this might have issues with duplicates?
    -- TODO: in production, this should probably run once and then never again.
    mapM_ (\migration -> runSqlPool (runMigration migration) pool) $ migrations plugin
    -- Create a var to kill any ongoing tasks.
    mvar <- newEmptyMVar :: IO (MVar [ThreadId])
    userFacingError <-
      runDiscord $
        def
          { discordToken = dToken,
            discordOnEvent =
              flip runSqlPool pool . eventHandler plugin prefix,
            discordOnStart =
              -- Build list of cron jobs, saving them to the mvar.
              runSqlPool (mapM runCron (cronJobs plugin) >>= liftIO . putMVar mvar) pool,
            -- Kill every cron job in the mvar.
            discordOnEnd = takeMVar mvar >>= killCron
          }
    TIO.putStrLn userFacingError
