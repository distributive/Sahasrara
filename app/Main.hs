module Main where

import Control.Monad (forever, unless)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import LoadEnv (loadEnv)
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import Tablebot (runTablebot)
import Tablebot.Plugins (plugins)
import Text.Regex.PCRE

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = forever $ do
  loadEnv
  dToken <- pack <$> getEnv "DISCORD_TOKEN"
  unless (encodeUtf8 dToken =~ ("^[A-Za-z0-9_-]{24}[.][A-Za-z0-9_-]{6}[.][A-Za-z0-9_-]{27}$" :: String)) $
    die "Invalid token format. Please check it is a bot token"
  prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
  dbpath <- getEnv "SQLITE_FILENAME"
  runTablebot dToken prefix dbpath plugins
