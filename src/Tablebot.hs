module Tablebot (
    runTablebot,
    Config(..)
) where

import Commands

import Data.Text
import Discord
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Redis

-- TODO: add Cron jobs somehow - maybe commands have registered bits and called bits?
data Config = Cfg { discordToken :: Text, prefix :: Text, rconn :: Connection, commands :: [Command] }

runTablebot :: Config -> IO ()
runTablebot cfg = do
    userFacingError <- runDiscord $ def {
        Discord.discordToken = Tablebot.discordToken cfg
    }
    TIO.putStrLn userFacingError