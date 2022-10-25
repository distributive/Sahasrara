-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Faction
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Faction and Factions types.
module Sahasrara.Plugins.Netrunner.Type.Faction where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Faction@ represents a Netrunner faction.
data Faction = Faction
  { code :: !Text,
    name :: !Text,
    description :: !Text,
    isMini :: !Bool,
    sideCode :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Faction where
  parseJSON = withObject "Faction" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    name <- a .: "name"
    description <- do
      desc <- a .:? "description"
      return $ fromMaybe "" desc
    isMini <- a .: "is_mini"
    sideCode <- a .: "side_id"
    return Faction {..}
