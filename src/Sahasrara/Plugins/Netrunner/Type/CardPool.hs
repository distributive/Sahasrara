-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Pack and Packs types.
module Sahasrara.Plugins.Netrunner.Type.CardPool where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @CardPool@ represents a card pool used by a format.
data CardPool = CardPool
  { code :: !Text,
    name :: !Text,
    cardCycleCodes :: ![Text],
    cardSetCodes :: ![Text],
    cardCodes :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardPool where
  parseJSON = withObject "CardPool" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    name <- a .: "name"
    cardCycleCodes <- a .: "card_cycle_ids"
    cardSetCodes <- a .: "card_set_ids"
    cardCodes <- a .: "card_ids"
    return CardPool {..}
