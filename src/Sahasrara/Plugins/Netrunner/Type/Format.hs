-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Glossary
-- Description : The type of the Netrunner glossary.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The format type.
module Sahasrara.Plugins.Netrunner.Type.Format where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Format@ represents a single format.
data Format = Format
  { code :: !Text,
    name :: !Text,
    activeSnapshotCode :: !Text,
    snapshotCodes :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Format where
  parseJSON = withObject "Format" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    name <- a .: "name"
    activeSnapshotCode <- a .: "active_snapshot_id"
    snapshotCodes <- a .: "snapshot_ids"
    return Format {..}
