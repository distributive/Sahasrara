-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Card and Cards types.
module Sahasrara.Plugins.ONR.Type.Card where

import Data.Aeson (FromJSON, Value (..), parseJSON, withObject, (.:?))
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Card@ represents a single card in the NetrunnerDB API.
data Card = Card
  { agendaPoints :: !(Maybe Int),
    code :: !(Maybe Text),
    cost :: !(Maybe Stat),
    difficulty :: !(Maybe Int),
    flavour :: !(Maybe Text),
    illustrator :: !(Maybe Text),
    memoryCost :: !(Maybe Int),
    rarity :: !(Maybe Text),
    set :: !(Maybe Text),
    sideCode :: !(Maybe Text),
    strength :: !(Maybe Stat),
    strippedText :: !(Maybe Text),
    strippedTitle :: !(Maybe Text),
    subtypes :: !(Maybe [Text]),
    text :: !(Maybe Text),
    title :: !(Maybe Text),
    trashCost :: !(Maybe Int),
    typeCode :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON Card where
  parseJSON = withObject "Card" $ \o ->
    Card <$> o .:? "agenda_points"
      <*> o .:? "code"
      <*> o .:? "cost"
      <*> o .:? "difficulty"
      <*> o .:? "flavour"
      <*> o .:? "illustrator"
      <*> o .:? "memory_cost"
      <*> o .:? "rarity"
      <*> o .:? "set"
      <*> o .:? "side_code"
      <*> o .:? "strength"
      <*> o .:? "stripped_text"
      <*> o .:? "stripped_title"
      <*> o .:? "subtypes"
      <*> o .:? "text"
      <*> o .:? "title"
      <*> o .:? "trash_cost"
      <*> o .:? "type_code"

-- | @Value@ represents values that are either a numeric value or a variable (usually X)
data Stat = Var Text | Val Int deriving (Show, Generic)

instance FromJSON Stat where
  parseJSON (String var) = Var <$> pure var
  parseJSON (Number val) = pure $ Val $ fromMaybe 0 $ toBoundedInteger val
  parseJSON _ = Var <$> pure ""

-- instance FromJSON [Text] where
--   parseJSON (Array arr) = [] <$> parseJSON
