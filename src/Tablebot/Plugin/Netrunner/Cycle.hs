module Tablebot.Plugin.Netrunner.Cycle (Cycle (..), Cycles (..), defaultCycles) where

import Data.Aeson (FromJSON, Value (Object), (.:), parseJSON)
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
data Cycles = Cycles { content :: ![Cycle] } deriving (Show, Generic)

defaultCycles :: Cycles
defaultCycles = Cycles { content = [] }

instance FromJSON Cycle
instance FromJSON Cycles where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Cycles { content = content }
  parseJSON _ = return defaultCycles
