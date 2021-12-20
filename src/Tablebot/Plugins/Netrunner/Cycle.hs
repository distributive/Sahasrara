-- |
-- Module      : Tablebot.Plugin.Netrunner.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cycles in Tablebot.
module Tablebot.Plugins.Netrunner.Cycle (Cycle (..), Cycles (..), defaultCycles) where

import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Cycle@ represents a single cycle of packs in the NetrunnerDB API.
data Cycle = Cycle
  { code :: !Text,
    name :: !Text,
    position :: !Int,
    size :: !Int,
    rotated :: !Bool
  }
  deriving (Show, Generic)

-- | @Cycles@ represents all cycles in the game's history.
newtype Cycles = Cycles {content :: [Cycle]} deriving (Show, Generic)

defaultCycles :: Cycles
defaultCycles = Cycles {content = []}

instance FromJSON Cycle

instance FromJSON Cycles where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Cycles {content = content}
  parseJSON _ = return defaultCycles
