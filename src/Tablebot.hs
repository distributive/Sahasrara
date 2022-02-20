{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Tablebot
-- Description : The main runner for the Tablebot Discord bot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
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
    newMVar,
    putMVar,
    takeMVar,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map as M
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Persist.Sqlite
  ( createSqlitePool,
    runMigration,
    runSqlPool,
  )
import Discord
import Discord.Internal.Rest
import Tablebot.Handler (eventHandler, killCron, runCron)
import Tablebot.Internal.Administration (adminMigration, currentBlacklist, removeBlacklisted)
import Tablebot.Internal.Plugins
import Tablebot.Internal.Types
import Tablebot.Utility
import Tablebot.Utility.Help

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
runTablebot :: VersionInfo -> Text -> Text -> FilePath -> [CompiledPlugin] -> IO ()
runTablebot vinfo dToken prefix dbpath plugins =
  do
    debugPrint ("DEBUG enabled. This is strongly not recommended in production!" :: String)
    -- Create multiple database threads.
    pool <- runNoLoggingT $ createSqlitePool (pack dbpath) 8

    -- Setup and then apply plugin blacklist from the database
    runSqlPool (runMigration adminMigration) pool
    blacklist <- runResourceT $ runNoLoggingT $ runSqlPool currentBlacklist pool
    let filteredPlugins = removeBlacklisted blacklist plugins
    -- Combine the list of plugins into both a combined plugin
    let !plugin = generateHelp $ combinePlugins filteredPlugins
    -- Run the setup actions of each plugin and collect the plugin actions into a single @PluginActions@ instance
    allActions <- mapM (runResourceT . runNoLoggingT . flip runSqlPool pool) (combinedSetupAction plugin)
    let !actions = combineActions allActions

    -- TODO: this might have issues with duplicates?
    -- TODO: in production, this should probably run once and then never again.
    mapM_ (\migration -> runSqlPool (runMigration migration) pool) $ combinedMigrations plugin
    -- Create a var to kill any ongoing tasks.
    mvar <- newEmptyMVar :: IO (MVar [ThreadId])
    cacheMVar <- newMVar (TCache M.empty vinfo) :: IO (MVar TablebotCache)
    userFacingError <-
      runDiscord $
        def
          { discordToken = dToken,
            discordOnEvent =
              flip runSqlPool pool . flip runReaderT cacheMVar . eventHandler actions prefix,
            discordOnStart = do
              -- Build list of cron jobs, saving them to the mvar.
              -- Note that we cannot just use @runSqlPool@ here - this creates
              -- a single transaction which is reverted in case of exception
              -- (which can just happen due to databases being unavailable
              -- sometimes).
              runReaderT (mapM (runCron pool) (compiledCronJobs actions) >>= liftIO . putMVar mvar) cacheMVar
              liftIO $ putStrLn "Tablebot lives!"
              sendCommand (UpdateStatus activityStatus),
            -- Kill every cron job in the mvar.
            discordOnEnd = takeMVar mvar >>= killCron
          }
    TIO.putStrLn userFacingError
  where
    activityStatus =
      UpdateStatusOpts
        { updateStatusOptsSince = Nothing,
          updateStatusOptsGame =
            Just
              ( Activity
                  { activityName = "with dice. Prefix is `" <> prefix <> "`. Call `" <> prefix <> "help` for help",
                    activityType = ActivityTypeGame,
                    activityUrl = Nothing
                  }
              ),
          updateStatusOptsNewStatus = UpdateStatusOnline,
          updateStatusOptsAFK = False
        }
