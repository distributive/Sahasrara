-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Card and Cards types.
module Sahasrara.Plugins.Netrunner.Type.Card where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn)
import GHC.Generics (Generic)
import Sahasrara.Plugins.Netrunner.Type.Stat (Stat)

-- | @Card@ represents a single card in the NetrunnerDB API.
data Card = Card
  { code :: !Text,
    strippedTitle :: !Text,
    title :: !Text,
    cardTypeCode :: !Text,
    sideCode :: !Text,
    factionCode :: !Text,
    advancementRequirement :: !(Maybe Stat),
    agendaPoints :: !(Maybe Int),
    baseLink :: !(Maybe Int),
    cost :: !(Maybe Stat),
    deckLimit :: !(Maybe Int),
    influenceCost :: !(Maybe Int),
    influenceLimit :: !(Maybe Int),
    memoryCost :: !(Maybe Int),
    minimumDeckSize :: !(Maybe Int),
    strength :: !(Maybe Stat),
    strippedText :: !Text,
    text :: !Text,
    trashCost :: !(Maybe Int),
    uniqueness :: !Bool,
    subtypes :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Card where
  parseJSON = withObject "Card" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    strippedTitle <- a .: "stripped_title"
    title <- a .: "title"
    cardTypeCode <- a .: "card_type_id"
    sideCode <- a .: "side_id"
    factionCode <- a .: "faction_id"
    advancementRequirement <- a .:? "advancement_requirement"
    agendaPoints <- a .:? "agenda_points"
    baseLink <- a .:? "base_link"
    cost <- a .:? "cost"
    deckLimit <- a .:? "deck_limit"
    influenceCost <- a .: "influence_cost"
    influenceLimit <- a .: "influence_limit"
    memoryCost <- a .:? "memory_cost"
    minimumDeckSize <- a .:? "minimum_deck_size"
    strength <- a .:? "strength"
    strippedText <- do
      (st :: Maybe Text) <- a .:? "stripped_text"
      return $ fromMaybe "" st
    text <- do
      (t :: Maybe Text) <- a .:? "text"
      return $ fromMaybe "" t
    trashCost <- a .:? "trash_cost"
    uniqueness <- a .: "is_unique"
    subtypes <- do
      s <- a .:? "display_subtypes"
      return $ case s of
        Nothing -> []
        Just s' -> splitOn " - " s'
    return Card {..}
