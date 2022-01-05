-- |
-- Module      : Tablebot.Plugins.Netrunner.Type.Faction
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The Faction and Factions types.
module Tablebot.Plugins.Netrunner.Type.Faction where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Faction@ represents a Netrunner faction.
data Faction = Faction
  { code :: !Text,
    colour :: !Text,
    isMini :: !Bool,
    name :: !Text,
    sideCode :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Faction where
  parseJSON = withObject "Faction" $ \o ->
    Faction <$> o .: "code"
      <*> o .: "color"
      <*> o .: "is_mini"
      <*> o .: "name"
      <*> o .: "side_code"
