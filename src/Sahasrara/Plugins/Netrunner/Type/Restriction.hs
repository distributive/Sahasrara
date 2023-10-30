-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Card and Cards types.
module Sahasrara.Plugins.Netrunner.Type.Restriction where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Map (Map, empty)
import Data.Text (Text)
-- import Data.Time
-- import Data.Time.Clock.POSIX
import GHC.Generics (Generic)

-- | @Restriction@ represents a single version of a Netrunner banlist/MWL/etc.
data Restriction = Restriction
  { code :: !Text,
    name :: !Text,
    dateStart :: !Text, -- !UTCTime,
    pointLimit :: !(Maybe Int),
    banned :: ![Text],
    restricted :: ![Text],
    universalFactionCost :: !(Map Text Int),
    globalPenalty :: ![Text],
    points :: !(Map Text Int),
    bannedSubtypes :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Restriction where
  parseJSON = withObject "Restriction" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    v <- a .: "verdicts"
    name <- a .: "name"
    dateStart <- a .: "date_start"
    -- dateStart <- do
    --   dateString <- a .: "date_start"
    --   return $ parseTimeOrError True defaultTimeLocale "%Y-%M-%D" dateString
    pointLimit <- a .: "point_limit"
    banned <- v .: "banned"
    restricted <- v .: "restricted"
    universalFactionCost <- v .: "universal_faction_cost"
    globalPenalty <- v .: "global_penalty"
    points <- v .: "points"
    bannedSubtypes <- a .: "banned_subtypes"
    return Restriction {..}

defaultRestriction :: Restriction
defaultRestriction =
  Restriction
    { code = "",
      name = "",
      dateStart = "", -- posixSecondsToUTCTime 0,
      pointLimit = Nothing,
      banned = [],
      restricted = [],
      universalFactionCost = empty,
      globalPenalty = [],
      points = empty,
      bannedSubtypes = []
    }
