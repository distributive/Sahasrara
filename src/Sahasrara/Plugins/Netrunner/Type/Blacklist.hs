-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Blacklist
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The blacklist for filtering horoscopes.
module Sahasrara.Plugins.Netrunner.Type.Blacklist where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Type@ represents a single card type in the NetrunnerDB API.
data Blacklist = Blacklist
  { badSubstrings :: ![Text],
    badCards :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Blacklist

defaultBlacklist :: Blacklist
defaultBlacklist = Blacklist {badSubstrings = [], badCards = []}
