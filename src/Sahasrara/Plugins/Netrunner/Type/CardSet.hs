-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Pack and Packs types.
module Sahasrara.Plugins.Netrunner.Type.CardSet where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Pack@ represents a single data pack in the NetrunnerDB API.
data CardSet = CardSet
  { code :: !Text,
    legacyCode :: !Text,
    name :: !Text,
    dateRelease :: !Text,
    size :: !Int,
    cycleCode :: !Text,
    cardSetTypeCode :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardSet where
  parseJSON = withObject "CardSet" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    legacyCode <- a .: "legacy_code"
    name <- a .: "name"
    dateRelease <- a .: "date_release"
    size <- a .: "size"
    cycleCode <- a .: "card_cycle_id"
    cardSetTypeCode <- a .: "card_set_type_id"
    return $ CardSet {..}
