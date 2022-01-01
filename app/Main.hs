module Main where

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Monad.Extra
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import LoadEnv (loadEnv)
import System.Environment (getEnv, lookupEnv)
import System.Process
import Tablebot (runTablebot)
import Tablebot.Internal.Administration
import Tablebot.Plugins (plugins)

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = do
  rFlag <- newMVar Reload :: IO (MVar ShutdownReason)
  whileM $ do
    _ <- swapMVar rFlag Reload
    loadEnv
    dToken <- pack <$> getEnv "DISCORD_TOKEN"
    prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
    dbpath <- getEnv "SQLITE_FILENAME"
    runTablebot dToken prefix dbpath (plugins rFlag)
    exit <- swapMVar rFlag Reload
    restartAction exit
    pure $ not (restartIsTerminal exit)
  putStrLn "Tablebot closed"
