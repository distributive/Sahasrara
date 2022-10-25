{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the jank command.
module Sahasrara.Plugins.Netrunner.Command.Jank (nrJank) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, toLower, unpack)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Card (Card (cardTypeCode, factionCode, sideCode))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Format (getStandard, toActiveSnapshot)
import Sahasrara.Plugins.Netrunner.Utility.Print (embedCards)
import Sahasrara.Plugins.Netrunner.Utility.Snapshot
import Sahasrara.Utility
import Sahasrara.Utility.Exception (BotException (GenericException), throwBot)
import Sahasrara.Utility.Random (chooseN, chooseOne)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))

data Side = Corp | Runner deriving (Show, Eq)

data Jank = Jank Card Card Card Card

-- | @nrJank@ is a command randomly generating an ID and three non-ID cards.
nrJank :: EnvCommand NrApi
nrJank = Command "jank" (parseComm jankComm) []
  where
    jankComm ::
      Either () (RestOfInput Text) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    jankComm (Left ()) m = do
      side <- liftIO $ chooseOne [Corp, Runner]
      jank <- generateJank side
      embedJank jank m
    jankComm (Right (ROI side)) m = do
      jank <- case toLower side of
        "corp" -> generateJank Corp
        "runner" -> generateJank Runner
        _ -> throwBot $ GenericException "Unrecognised side" $ "Could not match `" ++ (unpack side) ++ "` to `corp` or `runner`.\nGive no parameter to randomly choose a side"
      embedJank jank m
    pool :: NrApi -> Side -> [Card]
    pool api Corp = filter (("corp" ==) . sideCode) $ toLegalCards api $ toActiveSnapshot api $ getStandard api
    pool api Runner = filter (("runner" ==) . sideCode) $ toLegalCards api $ toActiveSnapshot api $ getStandard api
    generateJank :: Side -> EnvDatabaseDiscord NrApi Jank
    generateJank side = do
      api <- ask
      let idCode = if side == Corp then "corp_identity" else "runner_identity"
      identity <- liftIO $ chooseOne $ filter ((idCode ==) . cardTypeCode) $ pool api side
      [c1, c2, c3] <- liftIO $ chooseN 3 $ filter (\c -> (cardTypeCode c /= "agenda" || factionCode c == factionCode identity) && cardTypeCode c /= idCode) $ pool api side
      return $ Jank identity c1 c2 c3
    embedJank :: Jank -> Message -> EnvDatabaseDiscord NrApi ()
    embedJank (Jank i c1 c2 c3) m = embedCards "Your jank combo is:" "" [i, c1, c2, c3] "" "" m
