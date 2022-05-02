-- |
-- Module      : Sahasrara.Plugins.ONR.Type.OnrApi
-- Description : Handles the internal functionality of the ONR command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The OnrApi type.
module Sahasrara.Plugins.ONR.Type.OnrApi where

import Data.Text (Text)
import GHC.Generics (Generic)
import Sahasrara.Plugins.ONR.Type.Card (Card)

-- | @NrApi@ represents all required Netrunner data collected in one record.
data OnrApi = OnrApi
  { cards :: [Card],
    imageTemplate :: Text
  }
  deriving (Show, Generic)
