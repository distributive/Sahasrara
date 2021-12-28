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

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @BanList@ represents a single version of the Netrunner banlist.
data BanList = BanList
  { listId :: !Int,
    dateCreation :: !Text,
    dateUpdate :: !Text,
    code :: !Text,
    name :: !Text,
    active :: !Bool,
    dateStart :: !Text,
    affectedCards :: Map Text CardBan
  }
  deriving (Eq, Show, Generic)

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
-- This implementation assumes a card cannot be more than one of banned,
-- restricted, having a global penalty, or having universal influence.
data CardBan
  = Legal
  | GlobalPenalty !Int
  | UniversalInfluence !Int
  | Restricted
  | Banned
  deriving (Eq, Show, Generic)

instance FromJSON CardBan where
  parseJSON = withObject "CardBan" $ \o -> do
    globalPenalty <- do
      gp <- o .:? "global_penalty"
      return $ fromMaybe 0 gp
    universalInfluence <- do
      fc <- o .:? "universal_faction_cost"
      return $ fromMaybe 0 fc
    restricted <- do
      (restriction :: Maybe Int) <- o .:? "is_restricted"
      return $ maybe False (/= 0) restriction
    banned <- do
      (limit :: Maybe Int) <- o .:? "deck_limit"
      return $ maybe False (== 0) limit
    return $
      if
          | banned -> Banned
          | restricted -> Restricted
          | universalInfluence > 0 -> UniversalInfluence universalInfluence
          | globalPenalty > 0 -> GlobalPenalty globalPenalty
          | otherwise -> GlobalPenalty universalInfluence

defaultBanList :: BanList
defaultBanList =
  BanList
    { listId = -1,
      dateCreation = "",
      dateUpdate = "",
      code = "",
      name = "",
      active = False,
      dateStart = "",
      affectedCards = fromList []
    }
