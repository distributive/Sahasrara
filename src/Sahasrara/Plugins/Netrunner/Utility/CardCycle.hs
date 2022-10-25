-- |
-- Module      : Sahasrara.Plugins.Netrunner.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cycles in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.CardCycle where

import Data.Text (Text)
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle (cardSetCodes))
import qualified Sahasrara.Plugins.Netrunner.Type.CardCycle as CardCycle
import Sahasrara.Plugins.Netrunner.Type.CardSet (CardSet (cardSetTypeCode))
import qualified Sahasrara.Plugins.Netrunner.Type.CardSet as CardSet
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Format (getStandard, toActiveSnapshot)
import Sahasrara.Plugins.Netrunner.Utility.Legality (cycleLegality)
import Sahasrara.Plugins.Netrunner.Utility.Symbol (legalityToSymbol)

-- | @isCycle@ determines if a cycle is a "cycle" as opposed to a core set, big
-- box release, etc. represented internally as a CardCycle.
-- Returns true iff there is at least one CardSet belonging to the CardCycle
-- that is either a data pack or booster pack
isCycle :: NrApi -> CardCycle -> Bool
isCycle api c = any isValid $ cardSets api
  where
    isValid s = CardSet.cycleCode s == CardCycle.code c && cardSetTypeCode s `elem` ["data_pack", "booster_pack"]

-- | @isBigBox@ determines if a CardCycle represents a core set or deluxe
-- expansion.
-- Returns true iff there is at least one CardSet belonging to the CardCycle
-- that is either a core or deluxe release
isBigBox :: NrApi -> CardCycle -> Bool
isBigBox api c = any isValid $ cardSets api
  where
    isValid s = CardSet.cycleCode s == CardCycle.code c && cardSetTypeCode s `elem` ["core", "deluxe"]

-- | @isNsg@ determines if a cycle was released by Null Signal Games or FFG.
isNsg :: NrApi -> CardCycle -> Bool
isNsg api c = CardCycle.code c == "magnum_opus_reprint" || isCycle api c && (length $ cardSetCodes c) < 6

-- | @formatCycleNameLegality@ gets the name of a cycle and prefaces it with a
-- symbol representing its legality.
formatCycleNameLegality :: NrApi -> CardCycle -> Text
formatCycleNameLegality api c = (legalityToSymbol $ cycleLegality api (toActiveSnapshot api $ getStandard api) c) <> " " <> CardCycle.name c
