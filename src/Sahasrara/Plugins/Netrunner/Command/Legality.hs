{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the banHistory and banList commands.
module Sahasrara.Plugins.Netrunner.Command.Legality (nrRestriction) where

import Control.Monad.Trans.Reader (ask)
import Data.Maybe (catMaybes)
import Data.Text (Text, toLower, unpack)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Format (Format)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Restriction (Restriction (name))
import Sahasrara.Plugins.Netrunner.Utility.Format
import Sahasrara.Plugins.Netrunner.Utility.Print (embedRestriction, embedRestrictions)
import Sahasrara.Utility
import Sahasrara.Utility.Search (FuzzyCosts (..), closestValueWithCosts)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))

-- | @nrLegality@ is a command listing all cards affected by a restriction.
nrRestriction :: EnvCommand NrApi
nrRestriction = Command "banlist" (parseComm banListComm) []
  where
    banListComm ::
      Either () (RestOfInput Text) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    banListComm (Left ()) m = do
      api <- ask
      embedRestrictions (getStandard api) m
    banListComm (Right (ROI query)) m = do
      api <- ask
      case queryRestriction api query of
        Left format -> embedRestrictions format m
        Right restriction -> embedRestriction restriction m

-- | @queryRestriction@ matches the input to the restriction or valid format
-- with the closest name.
-- Valid formats are the big three: Standard, Startup, and Eternal
queryRestriction :: NrApi -> Text -> Either Format Restriction
queryRestriction api query =
  let rs = restrictions api
      standard = ("standard", Left $ getStandard api)
      startup = ("startup", Left $ getStartup api)
      eternal = ("eternal", Left $ getEternal api)
      active = (\r -> Just ("active", Right r)) =<< (toActiveRestriction api $ getStandard api)
      latest = (\r -> Just ("latest", Right r)) =<< (toLatestRestriction api $ getStandard api)
      pairs =
        standard :
        startup :
        eternal :
        catMaybes [active, latest]
          ++ zip (map (unpack . toLower . name) rs) (map Right rs)
   in closestValueWithCosts editCosts pairs $ unpack $ toLower query
  where
    editCosts =
      FuzzyCosts
        { deletion = 100,
          insertion = 1,
          substitution = 100,
          transposition = 100
        }
