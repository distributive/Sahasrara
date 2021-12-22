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

import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)
import Tablebot.Plugin.Discord (formatFromEmojiName)

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
  parseJSON (Object v) = do
    code <- v .: "code"
    colour <- v .: "color"
    isMini <- v .: "is_mini"
    name <- v .: "name"
    sideCode <- v .: "side_code"
    return $ Faction code colour isMini name sideCode

instance FromJSON Factions where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Factions {content = content}
  parseJSON _ = return defaultFactions
