-- |
-- Module      : Sahasrara.Plugins.Netrunner.Pack
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner packs in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.CardSet (toCycle, toCycleName) where

import Data.Text (Text)
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle (code), name)
import Sahasrara.Plugins.Netrunner.Type.CardSet (CardSet (cycleCode))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (cardCycles))

-- | @toCycle@ takes a pack and attempts to find its cycle.
toCycle :: NrApi -> CardSet -> Maybe CardCycle
toCycle api set =
  let cRes = filter (\c -> code c == cycleCode set) $ cardCycles api
   in case cRes of
        [] -> Nothing
        (c : _) -> Just c

toCycleName :: NrApi -> CardSet -> Maybe Text
toCycleName api set = name <$> toCycle api set
