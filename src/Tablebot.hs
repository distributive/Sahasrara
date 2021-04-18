module Tablebot (
    runTablebot,
    Config(..)
) where

import Plugin.Plugin

import Data.Text
import Discord
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Redis

data Config = Cfg { discordToken :: Text, prefix :: Text, rconn :: Connection, plugins :: [Plugin] }

runTablebot :: Config -> IO ()
runTablebot cfg = do
    userFacingError <- runDiscord $ def {
        Discord.discordToken = Tablebot.discordToken cfg,
        discordOnEvent = eventHandler (plugins cfg) (rconn cfg) (prefix cfg)
    }
    TIO.putStrLn userFacingError