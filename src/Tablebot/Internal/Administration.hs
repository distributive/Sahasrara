-- |
-- Module      : Tablebot.Internal.Administration
-- Description : Internal model and helpers for administrating the bot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Internal model and helpers for administrating the bot.
-- This is separate from the plugin as it needs to be accessed during plugin initialisation to disable plugins
module Tablebot.Internal.Administration
  ( module Tablebot.Internal.Administration,
    CompiledPlugin, -- Exfiltrate this to the admin plugin
  )
where

import Control.Monad.Cont (void, when)
import Data.List.Extra (isInfixOf, lower, trim)
import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Sqlite (SqlPersistM)
import Database.Persist.TH
import System.Environment (lookupEnv)
import System.Process
import Tablebot.Internal.Types

share
  [mkPersist sqlSettings, mkMigrate "adminMigration"]
  [persistLowerCase|
PluginBlacklist
    label String
    deriving Show
|]

currentBlacklist :: SqlPersistM [Text]
currentBlacklist = do
  bl <- selectList allBlacklisted []
  return $ fmap (pack . pluginBlacklistLabel . entityVal) bl
  where
    allBlacklisted :: [Filter PluginBlacklist]
    allBlacklisted = []

removeBlacklisted :: [Text] -> [CompiledPlugin] -> [CompiledPlugin]
removeBlacklisted bl = filter isNotBlacklisted
  where
    isNotBlacklisted p' = compiledName p' `notElem` bl

data ShutdownReason = Halt | Reload | Restart | GitUpdate

restartAction :: ShutdownReason -> IO ()
restartAction GitUpdate = do
  putStrLn "Git Update Requested"
  updateGit
  void $ spawnProcess "stack" ["run"]
restartAction Restart = do
  putStrLn "Restart Requested"
  void $ spawnProcess "stack" ["run"]
restartAction _ = return ()

restartIsTerminal :: ShutdownReason -> Bool
restartIsTerminal Reload = False
restartIsTerminal _ = True

updateGit :: IO ()
updateGit = do
  maybeEnabled <- lookupEnv "ALLOW_GIT_UPDATE"
  let enabled = maybe False ((== "true") . lower . trim) maybeEnabled
  when enabled $ do
    status <- readProcess "git" ["status"] ""
    let pattern :: String
        pattern = "working tree clean"
        clean :: Bool
        clean = isInfixOf pattern status
    if clean
      then do
        callProcess "git" ["pull", "--rebase"]
        pullStatus <- readProcess "git" ["status"] ""
        let pullClean :: Bool
            pullClean = isInfixOf pattern pullStatus
        if pullClean
          then putStrLn "Git pulled successfully. Restarting"
          else do
            putStrLn "Git pull failed. Please do it manually"
            callProcess "git" ["rebase", "--abort"]
      else putStrLn "Git directory not clean. Not updating"

gitVersion :: IO Text
gitVersion = (pack . trim) <$> readProcess "git" ["rev-parse", "HEAD"] ""
