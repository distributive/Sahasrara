-- |
-- Module      : Tablebot.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The Card and Cards types.
module Tablebot.Plugins.Netrunner.Type.Card where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:?))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Card@ represents a single card in the NetrunnerDB API.
data Card = Card
  { advancementCost :: !(Maybe Int),
    agendaPoints :: !(Maybe Int),
    baseLink :: !(Maybe Int),
    code :: !(Maybe Text),
    cost :: !(Maybe Int),
    deckLimit :: !(Maybe Int),
    factionCode :: !(Maybe Text),
    factionCost :: !(Maybe Int),
    flavour :: !(Maybe Text),
    illustrator :: !(Maybe Text),
    influenceLimit :: !(Maybe Int),
    keywords :: !(Maybe Text),
    memoryCost :: !(Maybe Int),
    minimumDeckSize :: !(Maybe Int),
    packCode :: !(Maybe Text),
    position :: !(Maybe Int),
    quantity :: !(Maybe Int),
    sideCode :: !(Maybe Text),
    strength :: !(Maybe Int),
    strippedText :: !(Maybe Text),
    strippedTitle :: !(Maybe Text),
    text :: !(Maybe Text),
    title :: !(Maybe Text),
    trashCost :: !(Maybe Int),
    typeCode :: !(Maybe Text),
    uniqueness :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON Card where
  parseJSON = withObject "Card" $ \o ->
    Card <$> o .:? "advancement_cost"
      <*> o .:? "agenda_points"
      <*> o .:? "base_link"
      <*> o .:? "code"
      <*> o .:? "cost"
      <*> o .:? "deck_limit"
      <*> o .:? "faction_code"
      <*> o .:? "faction_cost"
      <*> o .:? "flavor"
      <*> o .:? "illustrator"
      <*> o .:? "influence_limit"
      <*> o .:? "keywords"
      <*> o .:? "memory_cost"
      <*> o .:? "minimum_deck_size"
      <*> o .:? "pack_code"
      <*> o .:? "position"
      <*> o .:? "quantity"
      <*> o .:? "side_code"
      <*> o .:? "strength"
      <*> o .:? "stripped_text"
      <*> o .:? "stripped_title"
      <*> o .:? "text"
      <*> o .:? "title"
      <*> o .:? "trash_cost"
      <*> o .:? "type_code"
      <*> o .:? "uniqueness"
