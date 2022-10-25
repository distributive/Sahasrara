-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Glossary
-- Description : The type of the Netrunner glossary.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The representation of format snapshots.
module Sahasrara.Plugins.Netrunner.Type.Snapshot where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Text (Text)
-- import Data.Time
import GHC.Generics (Generic)

-- | @Snapshot@ represents a single snapshot of a format.
data Snapshot = Snapshot
  { code :: !Text,
    formatCode :: !Text,
    active :: !Bool,
    cardPoolCode :: !Text,
    restrictionCode :: !(Maybe Text),
    dateStart :: !Text -- !UTCTime
  }
  deriving (Eq, Show, Generic)

instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    formatCode <- a .: "format_id"
    active <- a .: "active"
    cardPoolCode <- a .: "card_pool_id"
    restrictionCode <- a .:? "restriction_id"
    dateStart <- a .: "date_start"
    -- dateStart <- do
    --   dateString <- a .: "date_start"
    --   return $ parseTimeOrError True defaultTimeLocale "%Y-%M-%D" dateString
    return Snapshot {..}
