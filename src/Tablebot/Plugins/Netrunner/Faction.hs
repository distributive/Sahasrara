-- |
-- Module      : Tablebot.Plugins.Netrunner.Faction
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner factions in Tablebot.
module Tablebot.Plugins.Netrunner.Faction (Faction (..), Factions (..), defaultFactions) where

import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Cycle@ represents a single cycle of packs in the NetrunnerDB API.
data Faction = Faction
  { code :: !Text,
    color :: !Text, -- American spelling to match the API
    is_mini :: !Bool,
    name :: !Text,
    side_code :: !Text
  }
  deriving (Show, Generic)

-- | @Cycles@ represents all cycles in the game's history.
newtype Factions = Factions {content :: [Faction]} deriving (Show, Generic)

defaultFactions :: Factions
defaultFactions = Factions {content = []}

instance FromJSON Faction

instance FromJSON Factions where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Factions {content = content}
  parseJSON _ = return defaultFactions
