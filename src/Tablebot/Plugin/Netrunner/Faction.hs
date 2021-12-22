-- |
-- Module      : Tablebot.Plugin.Netrunner.Faction
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner factions in Tablebot.
module Tablebot.Plugin.Netrunner.Faction (Faction (..), Factions (..), defaultFactions) where

import Data.Aeson (FromJSON, Value (Object), parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Cycle@ represents a single cycle of packs in the NetrunnerDB API.
data Faction = Faction
  { code :: !Text,
    colour :: !Text,
    isMini :: !Bool,
    name :: !Text,
    sideCode :: !Text
  }
  deriving (Show, Generic)

-- | @Cycles@ represents all cycles in the game's history.
data Factions = Factions {content :: ![Faction]} deriving (Show, Generic)

defaultFactions :: Factions
defaultFactions = Factions {content = []}

instance FromJSON Faction where
  parseJSON = withObject "Faction" $ \o ->
    Faction <$> o .: "code"
      <*> o .: "color"
      <*> o .: "is_mini"
      <*> o .: "name"
      <*> o .: "side_code"

instance FromJSON Factions where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Factions {content = content}
  parseJSON _ = return defaultFactions
