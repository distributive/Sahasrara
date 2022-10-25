-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Pack and Packs types.
module Sahasrara.Plugins.Netrunner.Type.CardType where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @CardType@ represents a single card type in the NetrunnerDB API.
data CardType = CardType
  { code :: !Text,
    name :: !Text,
    sideCode :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CardType where
  parseJSON = withObject "Type" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    name <- a .: "name"
    sideCode <- a .: "side_id"
    return CardType {..}
