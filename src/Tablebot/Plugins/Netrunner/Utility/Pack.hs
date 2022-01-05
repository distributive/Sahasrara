-- |
-- Module      : Tablebot.Plugins.Netrunner.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner packs in Tablebot.
module Tablebot.Plugins.Netrunner.Utility.Pack (toCycle) where

import Tablebot.Plugins.Netrunner.Type.Cycle (Cycle (code))
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (cycles))
import Tablebot.Plugins.Netrunner.Type.Pack (Pack (cycleCode))

-- | @toCycle@ takes a pack and attempts to find its cycle.
toCycle :: NrApi -> Pack -> Maybe Cycle
toCycle api pack' =
  let cRes = filter (\c -> code c == cycleCode pack') $ cycles api
   in case cRes of
        [] -> Nothing
        (c : _) -> Just c
