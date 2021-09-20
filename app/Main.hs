module Main where

import Tablebot
import Tablebot.Plugins (flipPlugin, pingPlugin, quotePlugin, reminderPlugin, sayPlugin, welcomePlugin)

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
    let plugins = [flipPlugin, pingPlugin, quotePlugin, reminderPlugin, sayPlugin, welcomePlugin]
    runTablebot dToken prefix dbpath plugins
