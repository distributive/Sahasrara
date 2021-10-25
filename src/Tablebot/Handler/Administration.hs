module Tablebot.Handler.Administration where

import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Sqlite (SqlPersistM)
import Database.Persist.TH
import Tablebot.Handler.Types

share
  [mkPersist sqlSettings, mkMigrate "adminMigration"]
  [persistLowerCase|
PluginBlacklist
    label String
    deriving Show
|]

currentBlacklist :: SqlPersistM ([Text])
currentBlacklist = do
  bl <- selectList allBlacklisted []
  return $ fmap (pack . pluginBlacklistLabel . unentity) bl
  where
    allBlacklisted :: [Filter PluginBlacklist]
    allBlacklisted = []
    unentity :: (Entity a) -> a
    unentity a = entityVal a

removeBlacklisted :: [Text] -> [CompiledPlugin] -> [CompiledPlugin]
removeBlacklisted bl p = filter isNotBlacklisted p
  where
    isNotBlacklisted p' = not (compliedName p' `elem` bl)
