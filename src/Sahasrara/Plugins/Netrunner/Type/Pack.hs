-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Pack and Packs types.
module Sahasrara.Plugins.Netrunner.Type.Pack where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Pack@ represents a single data pack in the NetrunnerDB API.
data Pack = Pack
  { code :: !Text,
    cycleCode :: !Text,
    name :: !Text,
    position :: !Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON Pack where
  parseJSON = withObject "Pack" $ \o ->
    Pack <$> o .: "code"
      <*> o .: "cycle_code"
      <*> o .: "name"
      <*> o .: "position"
