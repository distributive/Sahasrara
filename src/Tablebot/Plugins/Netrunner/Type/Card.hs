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

import Data.Aeson (FromJSON)
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
    memory_cost :: !(Maybe Int),
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

instance FromJSON Card
