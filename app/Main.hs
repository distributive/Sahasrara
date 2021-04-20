module Main where

import Tablebot
import Tablebot.Plugins (pingPlugin, quotePlugin)

import LoadEnv (loadEnv)
import System.Environment (getEnv, lookupEnv)
import Data.Text (pack)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    loadEnv
    dToken <- pack <$> getEnv "DISCORD_TOKEN"
    prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
    dbpath <- getEnv "SQLITE_FILENAME"
    let plugins = [pingPlugin, quotePlugin]
    runTablebot dToken prefix dbpath plugins