-- |
-- Module      : Sahasrara.Plugins.Netrunner.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner restrictions (banlists) in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.Restriction where

import Data.Text (Text)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (restrictions))
import Sahasrara.Plugins.Netrunner.Type.Restriction

-- | @fromRestrictionCode@ finds a restriction by its code.
fromRestrictionCode :: NrApi -> Text -> Maybe Restriction
fromRestrictionCode api c = case filter ((== c) . code) $ restrictions api of
  [] -> Nothing
  (x : _) -> Just x
