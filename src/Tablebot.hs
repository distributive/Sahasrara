module Tablebot (
    runTablebot
) where

import Plugin.Plugin

import Data.Text
import Discord
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Selda.SQLite

-- TODO (very far in future): choose between SQLite and Postgres.
runTablebot :: Text -> Text -> FilePath -> [Plugin SQLite] -> IO ()
runTablebot dToken prefix dbpath plugins = do
    userFacingError <- runDiscord $ def {
        discordToken = dToken,
        discordOnEvent = withSQLite dbpath . eventHandler plugins prefix
    }
    TIO.putStrLn userFacingError