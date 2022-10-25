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
import Sahasrara.Plugins.Netrunner.Type.Card (Card (title))
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle (code, name))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Print (embedCardSets, embedCycle, embedCycles, embedSets)
import Sahasrara.Utility
import Sahasrara.Utility.Search
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))

-- | @nrSets@ is a command that lists all packs a card was printed in.
nrSets :: EnvCommand NrApi
nrSets = Command "sets" (parseComm setsComm) []
  where
    setsComm :: RestOfInput Text -> Message -> EnvDatabaseDiscord NrApi ()
    setsComm (ROI query) m = case query of
      "" -> embedSets m
      _ -> do
        api <- ask
        case closestValue (cardsAndCycles api) $ unpack query of
          Left card -> embedCardSets card m
          Right cardCycle -> embedCycle_ cardCycle m
    cardsAndCycles :: NrApi -> [(String, Either Card CardCycle)]
    cardsAndCycles NrApi {cards = cards, cardCycles = cardCycles} =
      zip (map (unpack . title) cards) (map Left cards)
        ++ zip (map (unpack . name) cardCycles) (map Right cardCycles)
    embedCycle_ :: CardCycle -> Message -> EnvDatabaseDiscord NrApi ()
    embedCycle_ cardCycle m =
      let header = "♻️ **" <> name cardCycle <> "** ♻️"
          url = "https://netrunnerdb.com/en/cycle/" <> code cardCycle
       in embedCycle header url "" cardCycle m

-- | @nrCycles@ is a command that lists all cycles.
nrCycles :: EnvCommand NrApi
nrCycles = Command "cycles" (parseComm cyclesComm) []
  where
    cyclesComm :: Message -> EnvDatabaseDiscord NrApi ()
    cyclesComm m = embedCycles m
