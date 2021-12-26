-- |
-- Module      : Tablebot.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The Card and Cards types.
module Tablebot.Plugins.Netrunner.Type.BanList where

import Data.Map (Map, fromList)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @BanList@ represents a single version of the Netrunner banlist.
data BanList = BanList
  { id :: !Int,
    dateCreation :: !Text,
    dateUpdate :: !Text,
    code :: !Text,
    name :: !Text,
    active :: !Bool,
    dateStart :: !Text,
    affectedCards :: Map Text CardBan
  }
  deriving (Show, Generic)

instance FromJSON BanList where
  parseJSON = withObject "BanList" $ \o ->
    BanList <$> o .: "id"
      <*> o .: "date_creation"
      <*> o .: "date_update"
      <*> o .: "code"
      <*> o .: "name"
      <*> o .: "active"
      <*> o .: "date_start"
      <*> o .: "cards"

-- | @BanCard@ represents the ban status of a card
data CardBan = CardBan
  { globalPenalty :: !(Maybe Int),
    universalInfluence :: !(Maybe Int),
    isRestricted :: !(Maybe Bool),
    isBanned :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON CardBan where
  parseJSON = withObject "CardBan" $ \o -> do
    globalPenalty <- o .:? "global_penalty"
    universalInfluence <- o .:? "universal_faction_cost"
    isRestricted <- o .:? "is_restricted"
    isBanned <- do
      (limit :: Maybe Int) <- o .:? "deck_limit"
      return $ case limit of
        Just 0 -> Just True
        Just _ -> Just False
        Nothing -> Nothing
    return CardBan {..}

defaultBanList :: BanList
defaultBanList = BanList
  { id = -1,
    dateCreation = "",
    dateUpdate = "",
    code = "",
    name = "",
    active = False,
    dateStart = "",
    affectedCards = fromList []
  }
