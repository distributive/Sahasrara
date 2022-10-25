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
import Sahasrara.Plugins.Netrunner.Type.Blacklist (Blacklist)
import Sahasrara.Plugins.Netrunner.Type.Card (Card)
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle)
import Sahasrara.Plugins.Netrunner.Type.CardPool (CardPool)
import Sahasrara.Plugins.Netrunner.Type.CardSet (CardSet)
import Sahasrara.Plugins.Netrunner.Type.CardType (CardType)
import Sahasrara.Plugins.Netrunner.Type.Faction (Faction)
import Sahasrara.Plugins.Netrunner.Type.Format (Format)
import Sahasrara.Plugins.Netrunner.Type.Glossary (Glossary)
import Sahasrara.Plugins.Netrunner.Type.Printing (Printing)
import Sahasrara.Plugins.Netrunner.Type.Restriction (Restriction)
import Sahasrara.Plugins.Netrunner.Type.Snapshot (Snapshot)

-- | @NrApi@ represents all required Netrunner data collected in one record.
data NrApi = NrApi
  { cards :: [Card],
    printings :: [Printing],
    cardTypes :: [CardType],
    factions :: [Faction],
    cardCycles :: [CardCycle],
    cardSets :: [CardSet],
    formats :: [Format],
    snapshots :: [Snapshot],
    cardPools :: [CardPool],
    restrictions :: [Restriction],
    cardAliases :: Map Text Text,
    blacklist :: Blacklist,
    glossary :: Glossary
  }
  deriving (Eq, Show, Generic)
