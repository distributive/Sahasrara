-- |
-- Module      : Tablebot.Internal.Administration
-- Description : Internal model and helpers for administrating the bot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Internal model and helpers for administrating the bot.
-- This is separate from the plugin as it needs to be accessed during plugin initialisation to disable plugins
module Tablebot.Internal.Administration
  ( module Tablebot.Internal.Administration,
    CompiledPlugin, -- Exfiltrate this to the admin plugin
  )
where

import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Sqlite (SqlPersistM)
import Database.Persist.TH
import Tablebot.Internal.Types

share
  [mkPersist sqlSettings, mkMigrate "adminMigration"]
  [persistLowerCase|
PluginBlacklist
    label String
    deriving Show
|]

currentBlacklist :: SqlPersistM [Text]
currentBlacklist = do
  bl <- selectList allBlacklisted []
  return $ fmap (pack . pluginBlacklistLabel . entityVal) bl
  where
    allBlacklisted :: [Filter PluginBlacklist]
    allBlacklisted = []

removeBlacklisted :: [Text] -> [CompiledPlugin] -> [CompiledPlugin]
removeBlacklisted bl = filter isNotBlacklisted
  where
    isNotBlacklisted p' = compiledName p' `notElem` bl
