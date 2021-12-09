module Tablebot.Plugin.Netrunner.Card (Card (..), Cards (..), defaultCards) where

import Data.Aeson (FromJSON, Value (Object), (.:), parseJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Card@ represents a single card in the NetrunnerDB API.
data Card = Card
  { advancement_cost :: !(Maybe Int),
    agenda_points :: !(Maybe Int),
    base_link :: !(Maybe Int),
    code :: !Text,
    cost :: !(Maybe Int),
    deck_limit :: !Int,
    faction_code :: !Text,
    faction_cost :: !Int,
    flavor :: !(Maybe Text),
    illustrator :: !(Maybe Text),
    influence_limit :: !(Maybe Int),
    keywords :: !(Maybe Text),
    minimum_deck_size :: !(Maybe Int),
    pack_code :: !Text,
    position :: !Int,
    quantity :: !Int,
    side_code :: !Text,
    strength :: !(Maybe Int),
    stripped_text :: !(Maybe Text),
    stripped_title :: !Text,
    text :: !(Maybe Text),
    title :: !Text,
    trash_cost :: !(Maybe Int),
    type_code :: !Text,
    uniqueness :: !Bool
  }
  deriving (Show, Generic)

-- | @Cards@ represents the full library of cards in Netrunner.
data Cards = Cards
  { content :: ![Card],
    imageUrlTemplate :: !Text
  }
  deriving (Show, Generic)

defaultCards :: Cards
defaultCards = Cards { content = [], imageUrlTemplate = "" }

instance FromJSON Card
instance FromJSON Cards where
  parseJSON (Object v) = do
    content <- v .: "data"
    imageUrlTemplate <- v .: "imageUrlTemplate"
    return $ Cards { content = content, imageUrlTemplate = imageUrlTemplate }
  parseJSON _ = return defaultCards
