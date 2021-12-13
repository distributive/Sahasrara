-- |
-- Module      : Tablebot.Plugin.Netrunner.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cards in Tablebot.
module Tablebot.Plugin.Netrunner.Card (Card (..), Cards (..), defaultCards) where

import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Card@ represents a single card in the NetrunnerDB API.
data Card = Card
  { advancement_cost :: !(Maybe Int),
    agenda_points :: !(Maybe Int),
    base_link :: !(Maybe Int),
    code :: !(Maybe Text),
    cost :: !(Maybe Int),
    deck_limit :: !(Maybe Int),
    faction_code :: !(Maybe Text),
    faction_cost :: !(Maybe Int),
    flavor :: !(Maybe Text),
    illustrator :: !(Maybe Text),
    influence_limit :: !(Maybe Int),
    keywords :: !(Maybe Text),
    minimum_deck_size :: !(Maybe Int),
    pack_code :: !(Maybe Text),
    position :: !(Maybe Int),
    quantity :: !(Maybe Int),
    side_code :: !(Maybe Text),
    strength :: !(Maybe Int),
    stripped_text :: !(Maybe Text),
    stripped_title :: !(Maybe Text),
    text :: !(Maybe Text),
    title :: !(Maybe Text),
    trash_cost :: !(Maybe Int),
    type_code :: !(Maybe Text),
    uniqueness :: !(Maybe Bool)
  }
  deriving (Show, Generic)

-- | @Cards@ represents the full library of cards in Netrunner.
data Cards = Cards
  { content :: ![Card],
    imageUrlTemplate :: !Text
  }
  deriving (Show, Generic)

defaultCards :: Cards
defaultCards = Cards {content = [], imageUrlTemplate = ""}

instance FromJSON Card

instance FromJSON Cards where
  parseJSON (Object v) = do
    content <- v .: "data"
    imageUrlTemplate <- v .: "imageUrlTemplate"
    return $ Cards {content = content, imageUrlTemplate = imageUrlTemplate}
  parseJSON _ = return defaultCards
