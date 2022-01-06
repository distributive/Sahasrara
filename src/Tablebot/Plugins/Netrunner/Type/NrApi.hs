-- |
-- Module      : Tablebot.Plugins.Netrunner.Type.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The NrApi type.
module Tablebot.Plugins.Netrunner.Type.NrApi where

import Data.Text (Text)
import GHC.Generics (Generic)
import Tablebot.Plugins.Netrunner.Type.BanList (BanList)
import Tablebot.Plugins.Netrunner.Type.Card (Card)
import Tablebot.Plugins.Netrunner.Type.Cycle (Cycle)
import Tablebot.Plugins.Netrunner.Type.Faction (Faction)
import Tablebot.Plugins.Netrunner.Type.Pack (Pack)
import Tablebot.Plugins.Netrunner.Type.Type (Type)

-- | @NrApi@ represents all required Netrunner data collected in one record.
data NrApi = NrApi
  { cards :: [Card],
    imageTemplate :: Text,
    types :: [Type],
    factions :: [Faction],
    cycles :: [Cycle],
    packs :: [Pack],
    banLists :: [BanList]
  }
  deriving (Show, Generic)
