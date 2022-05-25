-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The NrApi type.
module Sahasrara.Plugins.Netrunner.Type.NrApi where

import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sahasrara.Plugins.Netrunner.Type.BanList (BanList)
import Sahasrara.Plugins.Netrunner.Type.Blacklist (Blacklist)
import Sahasrara.Plugins.Netrunner.Type.Card (Card)
import Sahasrara.Plugins.Netrunner.Type.Cycle (Cycle)
import Sahasrara.Plugins.Netrunner.Type.Faction (Faction)
import Sahasrara.Plugins.Netrunner.Type.Glossary (Glossary)
import Sahasrara.Plugins.Netrunner.Type.Pack (Pack)
import Sahasrara.Plugins.Netrunner.Type.Type (Type)

-- | @NrApi@ represents all required Netrunner data collected in one record.
data NrApi = NrApi
  { cards :: [Card],
    imageTemplate :: Text,
    types :: [Type],
    factions :: [Faction],
    cycles :: [Cycle],
    packs :: [Pack],
    banLists :: [BanList],
    cardAliases :: Map Text Text,
    blacklist :: Blacklist,
    glossary :: Glossary
  }
  deriving (Eq, Show, Generic)
