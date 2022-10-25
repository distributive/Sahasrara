-- |
-- Module      : Sahasrara.Plugins.Netrunner.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner formats.
module Sahasrara.Plugins.Netrunner.Utility.Format where

import Data.List (maximumBy, nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Safe
import Sahasrara.Plugins.Netrunner.Type.CardPool (CardPool)
import Sahasrara.Plugins.Netrunner.Type.Format (Format (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Restriction (Restriction)
import qualified Sahasrara.Plugins.Netrunner.Type.Restriction as Restriction
import Sahasrara.Plugins.Netrunner.Type.Snapshot (Snapshot (cardPoolCode, dateStart, formatCode, restrictionCode))
import Sahasrara.Plugins.Netrunner.Utility.CardPool (fromCardPoolCode, fromCardPoolCodeUnsafe)
import Sahasrara.Plugins.Netrunner.Utility.Restriction (fromRestrictionCode)
import Sahasrara.Plugins.Netrunner.Utility.Snapshot (fromSnapshotCode, fromSnapshotCodeUnsafe, toRestriction)
import Prelude hiding (lookup)

-- | @getStandard@ gets the Standard format.
-- It assumes it exists with code "standard" and will break otherwise -- TODO: probs change this
getStandard :: NrApi -> Format
getStandard api = fromFormatCodeUnsafe api "standard"

-- | @getStartup@ gets the Startup format.
-- It assumes it exists with code "startup" and will break otherwise -- TODO: and this
getStartup :: NrApi -> Format
getStartup api = fromFormatCodeUnsafe api "startup"

-- | @getEternal@ gets the Eternal format.
-- It assumes it exists with code "eternal" and will break otherwise -- TODO: this too
getEternal :: NrApi -> Format
getEternal api = fromFormatCodeUnsafe api "eternal"

-- | @fromFormatCode@ finds a format by its code.
fromFormatCode :: NrApi -> Text -> Maybe Format
fromFormatCode api c = case filter ((== c) . code) $ formats api of
  [] -> Nothing
  (x : _) -> Just x

-- | @fromFormatCodeUnsafe@ finds a format by its code.
-- This assumes there is one and shouldn't be used unless you're sure there is
fromFormatCodeUnsafe :: NrApi -> Text -> Format
fromFormatCodeUnsafe api c = headNote "4" $ filter ((== c) . code) $ formats api

-- | @fromSnapshot@ gets a snapshot's format.
fromSnapshot :: NrApi -> Snapshot -> Format
fromSnapshot api snapshot = fromFormatCodeUnsafe api $ formatCode snapshot

-- | @toActiveSnapshot@ gets the active snapshot of a format.
toActiveSnapshot :: NrApi -> Format -> Snapshot
toActiveSnapshot api format = fromSnapshotCodeUnsafe api $ activeSnapshotCode format

-- | @toSnapshots@ lists all snapshots in a format.
toSnapshots :: NrApi -> Format -> [Snapshot]
toSnapshots api Format {snapshotCodes = snapshotCodes} =
  mapMaybe (fromSnapshotCode api) snapshotCodes

-- | @toActiveCardPool@ gets the active card pool of a format.
toActiveCardPool :: NrApi -> Format -> CardPool
toActiveCardPool api format = fromCardPoolCodeUnsafe api $ activeSnapshotCode format

-- | @toCardPools@ lists all card pools in a format.
toCardPools :: NrApi -> Format -> [CardPool]
toCardPools api format =
  mapMaybe ((fromCardPoolCode api) . cardPoolCode) $ toSnapshots api format

-- | @toRestrictions@ lists all restrictions in a format.
toRestrictions :: NrApi -> Format -> [Restriction]
toRestrictions api format =
  mapMaybe (fromRestrictionCode api) $ nub $ mapMaybe restrictionCode $ toSnapshots api format

-- | @toActiveRestriction@ gets a format's active restriction.
toActiveRestriction :: NrApi -> Format -> Maybe Restriction
toActiveRestriction api format = toRestriction api $ toActiveSnapshot api format

-- | @toLatestRestriction@ gets the latest restriction used by a given format.
-- Assumes restrictions are ordered newest first
-- Of the latest snapshot has no restriction, the empty restriction is given
toLatestRestriction :: NrApi -> Format -> Maybe Restriction
toLatestRestriction api format = case toSnapshots api format of
  [] -> Nothing
  ss -> toRestriction api $ last ss

-- | @isActiveRestriction@ determines if a given restriction is active in a
-- given format.
isActiveRestriction :: NrApi -> Format -> Restriction -> Bool
isActiveRestriction api format r = toActiveRestriction api format == Just r

-- | @isLatestRestrictionActive@ checks if the latest restriction of a format is
-- active.
isLatestRestrictionActive :: Format -> Bool
isLatestRestrictionActive Format {snapshotCodes = codes, activeSnapshotCode = active} =
  codes == [] || last codes == active

-- | @lastSnapshotUsing@ gets the last snapshot using the given restriction.
lastSnapshotUsing :: NrApi -> Format -> Restriction -> Maybe Snapshot
lastSnapshotUsing api format r =
  case filter ((== (Just $ Restriction.code r)) . restrictionCode) $ toSnapshots api format of
    [] -> Nothing
    ss -> Just $ maximumBy (\a b -> compare (dateStart a) (dateStart b)) ss
