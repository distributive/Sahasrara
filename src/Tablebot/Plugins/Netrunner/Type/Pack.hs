-- |
-- Module      : Tablebot.Plugins.Netrunner.Type.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The Pack and Packs types.
module Tablebot.Plugins.Netrunner.Type.Pack where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Pack@ represents a single data pack in the NetrunnerDB API.
data Pack = Pack
  { code :: !Text,
    cycle_code :: !Text,
    name :: !Text,
    position :: !Int
  }
  deriving (Show, Generic)

instance FromJSON Pack
