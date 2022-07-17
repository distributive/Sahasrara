{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for commands displaying card sets.
module Sahasrara.Plugins.Netrunner.Command.Sets (nrSets, nrCycles) where

import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, unpack)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Cycle (Cycle (code, name, rotated))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Print (embedCardSets, embedCycles, embedSets, embedSetsOn)
import Sahasrara.Utility
import Sahasrara.Utility.Search (closestValue)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))

-- | @nrSets@ is a command that lists all packs a card was printed in.
nrSets :: EnvCommand NrApi
nrSets = Command "sets" (parseComm setsComm) []
  where
    setsComm :: RestOfInput Text -> Message -> EnvDatabaseDiscord NrApi ()
    setsComm (ROI card) m = case card of
      "" -> embedSets m
      _ -> embedCardSets card m

-- | @nrCycles@ is a command that lists the packs in a cycle
nrCycles :: EnvCommand NrApi
nrCycles = Command "cycles" (parseComm cyclesComm) []
  where
    cyclesComm :: RestOfInput Text -> Message -> EnvDatabaseDiscord NrApi ()
    cyclesComm (ROI cy) m = case cy of
      "" -> embedCycles m
      c -> do
        api <- ask
        let pairs = zip (map (unpack . standardise . name) $ cycles api) (cycles api)
            closestCycle = closestValue pairs $ unpack $ standardise c
            title = ":recycle: **" <> name closestCycle <> "** :recycle:"
            url = "https://netrunnerdb.com/en/cycle/" <> code closestCycle
            pre = if rotated closestCycle then "**Rotated**" else ""
        embedSetsOn title url pre (\c' -> c' == closestCycle) m
