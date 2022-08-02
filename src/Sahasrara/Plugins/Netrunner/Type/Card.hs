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

import Data.Aeson (FromJSON, Value (..), parseJSON, withObject, (.:!), (.:?))
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sahasrara.Utility

-- | @Card@ represents a single card in the NetrunnerDB API.
data Card = Card
  { advancementCost :: !(Maybe Stat),
    agendaPoints :: !(Maybe Int),
    baseLink :: !(Maybe Int),
    code :: !(Maybe Text),
    cost :: !(Maybe Stat),
    deckLimit :: !(Maybe Int),
    factionCode :: !(Maybe Text),
    factionCost :: !(Maybe Int),
    flavour :: !(Maybe Text),
    illustrator :: !(Maybe Text),
    influenceLimit :: !(Maybe Int),
    subtypes :: !(Maybe Text),
    memoryCost :: !(Maybe Int),
    minimumDeckSize :: !(Maybe Int),
    packCode :: !(Maybe Text),
    position :: !(Maybe Int),
    quantity :: !(Maybe Int),
    sideCode :: !(Maybe Text),
    strength :: !(Maybe Stat),
    strippedText :: !(Maybe Text),
    strippedTitle :: !(Maybe Text),
    text :: !(Maybe Text),
    title :: !(Maybe Text),
    trashCost :: !(Maybe Int),
    typeCode :: !(Maybe Text),
    uniqueness :: !(Maybe Bool)
  }
  deriving (Eq, Show, Generic)

-- Note: (.:?) is not used for (Maybe Stat) fields since nrdb represents
-- variables as null.
instance FromJSON Card where
  parseJSON = withObject "Card" $ \o ->
    Card <$> o .:! "advancement_cost"
      <*> o .:? "agenda_points"
      <*> o .:? "base_link"
      <*> o .:? "code"
      <*> o .:! "cost"
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
      <*> o .:! "strength"
      <*> o .:? "stripped_text"
      <*> o .:? "stripped_title"
      <*> o .:? "text"
      <*> o .:? "title"
      <*> o .:? "trash_cost"
      <*> o .:? "type_code"
      <*> o .:? "uniqueness"

-- | @Stat@ represents values that are either a numeric value or a variable (usually X)
data Stat = Var Text | Val Int deriving (Eq, Show, Generic)

statToText :: Stat -> Text
statToText (Var x) = x
statToText (Val x) = intToText x

isVar :: Stat -> Bool
isVar (Var _) = True
isVar _ = False

isVal :: Stat -> Bool
isVal (Val _) = True
isVal _ = False

instance FromJSON Stat where
  parseJSON (String var) = Var <$> pure var
  parseJSON (Number val) = pure $ Val $ fromMaybe 0 $ toBoundedInteger val
  parseJSON _ = Var <$> pure "X"
