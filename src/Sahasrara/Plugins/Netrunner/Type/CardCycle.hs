-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Cycle and Cycles types.
module Sahasrara.Plugins.Netrunner.Type.CardCycle where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Cycle@ represents a single cycle of packs in the NetrunnerDB API.
data CardCycle = CardCycle
  { code :: !Text,
    legacyCode :: !Text,
    name :: !Text,
    dateRelease :: !Text,
    cardSetCodes :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardCycle where
  parseJSON = withObject "Cycle" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    legacyCode <- a .: "legacy_code"
    name <- a .: "name"
    dateRelease <- a .: "date_release"
    cardSetCodes <- a .: "card_set_ids"
    return CardCycle {..}
