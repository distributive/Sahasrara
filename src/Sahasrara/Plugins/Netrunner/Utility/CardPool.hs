-- |
-- Module      : Sahasrara.Plugins.Netrunner.Utility.Snapshot
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner snapshots in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.CardPool where

import Data.Text (Text)
import Safe
import Sahasrara.Plugins.Netrunner.Type.CardPool (CardPool (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (cardPools))

-- | @fromCardPoolCode@ finds a snapshot by its code.
fromCardPoolCode :: NrApi -> Text -> Maybe CardPool
fromCardPoolCode api c = case filter ((== c) . code) $ cardPools api of
  [] -> Nothing
  (x : _) -> Just x

-- | @fromCardPoolCodeUnsafe@ finds a snapshot by its code.
-- This assumes there is one and shouldn't be used unless you're sure there is
fromCardPoolCodeUnsafe :: NrApi -> Text -> CardPool
fromCardPoolCodeUnsafe api c = headNote "3" $ filter ((== c) . code) $ cardPools api
