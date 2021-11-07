module Main where

import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import LoadEnv (loadEnv)
import System.Environment (getEnv, lookupEnv)
import Tablebot (runTablebot)
import Tablebot.Plugins (plugins)

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = forever $ do
  loadEnv
  dToken <- pack <$> getEnv "DISCORD_TOKEN"
  prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
  dbpath <- getEnv "SQLITE_FILENAME"
  runTablebot dToken prefix dbpath plugins
