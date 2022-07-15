{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Sahasrara
-- Description : The main runner for the Sahasrara Discord bot.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the main runner for Sahasrara. If you're just looking to
-- run the bot with existing plugins, importing this and your favourite plugins
-- from "Sahasrara.Plugins".
module Sahasrara
  ( runSahasrara,
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
import Sahasrara.Handler (eventHandler, killCron, runCron)
import Sahasrara.Internal.Administration (adminMigration, currentBlacklist, removeBlacklisted)
import Sahasrara.Internal.Plugins
import Sahasrara.Internal.Types
import Sahasrara.Utility
import Sahasrara.Utility.Help

-- | runSahasrara @dToken@ @prefix@ @dbpath@ @plugins@ runs the bot using the
-- given Discord API token @dToken@ and SQLite connection string @dbpath@. Only
-- the plugins provided by @plugins@ are run, and all commands are prefixed
-- with @prefix@.
-- The plugins given are combined into a single plugin with their combined
-- functionality. Each migration present in the combined plugin is run, and
-- each cron job and handler is set up.
-- This creates a small pool of database connections used by the event handler,
-- builds an event handler and starts cron jobs. It also kills the cron jobs on
-- bot close.
runSahasrara :: VersionInfo -> Text -> Text -> FilePath -> [CompiledPlugin] -> IO ()
runSahasrara vinfo dToken prefix dbpath plugins =
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
    cacheMVar <- newMVar (TCache M.empty vinfo) :: IO (MVar SahasraraCache)
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
              liftIO $ putStrLn "Sahasrara lives!"
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
                  { activityName = "Netrunner. Type " <> prefix <> "help for help. Prefix is " <> prefix <> ".",
                    activityType = ActivityTypeGame,
                    activityUrl = Nothing
                  }
              ),
          updateStatusOptsNewStatus = UpdateStatusOnline,
          updateStatusOptsAFK = False
        }
