{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Sahasrara (BotConfig (..), runSahasraraWithEnv)
import Sahasrara.Plugins (allPlugins)
import Sahasrara.Utility.Help (rootBody)

-- @main@ runs forever. This allows bot reloading by fully shutting down the bot and letting it restart.
main :: IO ()
main = runSahasraraWithEnv allPlugins $ BotConfig {gamePlaying = "!!!", rootHelpText = rootBody}
