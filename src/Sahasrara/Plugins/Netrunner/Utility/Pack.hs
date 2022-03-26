-- |
-- Module      : Sahasrara.Plugins.Netrunner.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner packs in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.Pack (toCycle) where

import Sahasrara.Plugins.Netrunner.Type.Cycle (Cycle (code))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (cycles))
import Sahasrara.Plugins.Netrunner.Type.Pack (Pack (cycleCode))

-- | @toCycle@ takes a pack and attempts to find its cycle.
toCycle :: NrApi -> Pack -> Maybe Cycle
toCycle api pack' =
  let cRes = filter (\c -> code c == cycleCode pack') $ cycles api
   in case cRes of
        [] -> Nothing
        (c : _) -> Just c
