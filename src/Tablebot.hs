module Tablebot (
    runTablebot,
    Config(..)
) where

import Commands

import Data.Text
import Discord
import qualified Data.Text.IO as TIO (putStrLn)

data Config = Cfg { discordToken :: Text, commands :: [Command] }

runTablebot :: Config -> IO ()
runTablebot cfg = do
    userFacingError <- runDiscord $ def {
        Discord.discordToken = Tablebot.discordToken cfg
    }
    TIO.putStrLn userFacingError