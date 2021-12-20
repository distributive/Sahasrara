-- |
-- Module      : Tablebot.Plugin.Netrunner.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner packs in Tablebot.
module Tablebot.Plugins.Netrunner.Pack (Pack (..), Packs (..), defaultPacks) where

import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
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

-- | @Packs@ represents all data packs in the game's history.
newtype Packs = Packs {content :: [Pack]} deriving (Show, Generic)

defaultPacks :: Packs
defaultPacks = Packs {content = []}

instance FromJSON Pack

instance FromJSON Packs where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Packs {content = content}
  parseJSON _ = return defaultPacks
