-- |
-- Module      : Sahasrara.Plugins.Netrunner.Utility.Snapshot
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner snapshots in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.Snapshot where

import Data.Text (Text)
import Safe
import Sahasrara.Plugins.Netrunner.Type.Card (Card)
import qualified Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.CardPool (CardPool)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (snapshots))
import Sahasrara.Plugins.Netrunner.Type.Restriction (Restriction (banned))
import Sahasrara.Plugins.Netrunner.Type.Snapshot (Snapshot (..))
import qualified Sahasrara.Plugins.Netrunner.Type.Snapshot as Snapshot
import Sahasrara.Plugins.Netrunner.Utility.CardPool (fromCardPoolCodeUnsafe, toCards)
import Sahasrara.Plugins.Netrunner.Utility.Restriction (fromRestrictionCode)

-- | @fromSnapshotCode@ finds a snapshot by its code.
fromSnapshotCode :: NrApi -> Text -> Maybe Snapshot
fromSnapshotCode api c = case filter ((== c) . Snapshot.code) $ snapshots api of
  [] -> Nothing
  (x : _) -> Just x

-- | @fromSnapshotCodeUnsafe@ finds a snapshot by its code.
-- This assumes there is one and shouldn't be used unless you're sure there is
fromSnapshotCodeUnsafe :: NrApi -> Text -> Snapshot
fromSnapshotCodeUnsafe api c = headNote "10" $ filter ((== c) . Snapshot.code) $ snapshots api

-- | @toCardPool@ gets a snapshot's cardpool.
toCardPool :: NrApi -> Snapshot -> CardPool
toCardPool api snapshot = fromCardPoolCodeUnsafe api $ cardPoolCode snapshot

-- | @toRestriction@ gets a snapshot's restriction.
toRestriction :: NrApi -> Snapshot -> Maybe Restriction
toRestriction api snapshot = fromRestrictionCode api =<< restrictionCode snapshot

-- | @toLegalCards@ gets the cards legal to a snapshot
toLegalCards :: NrApi -> Snapshot -> [Card]
toLegalCards api snapshot =
  let bannedCodes = case toRestriction api snapshot of
        Nothing -> []
        Just r -> banned r
   in filter (not . (`elem` bannedCodes) . Card.code) $ toCards api $ toCardPool api snapshot
