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
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text, toLower, unpack)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Card (Card (factionCode, packCode, sideCode, title, typeCode))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Print (embedCards)
import Sahasrara.Utility
import Sahasrara.Utility.Exception (BotException (GenericException), throwBot)
import Sahasrara.Utility.Random (chooseN, chooseOne)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))

data Side = Corp | Runner

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
    generateJank :: Side -> EnvDatabaseDiscord NrApi Jank
    generateJank Corp = do
      api <- ask
      let pool = nubBy (\c1 c2 -> title c1 == title c2) $ filter (\c -> sideCode c == Just "corp" && (not $ elem (fromMaybe "" $ packCode c) ["draft", "napd", "tdc"])) $ cards api
      identity <- liftIO $ chooseOne $ filter (\c -> typeCode c == Just "identity" && factionCode c /= Just "neutral") pool
      [c1, c2, c3] <- liftIO $ chooseN 3 $ filter (\c -> (typeCode c /= Just "agenda" || factionCode c == factionCode identity) && typeCode c /= Just "identity") pool
      return $ Jank identity c1 c2 c3
    generateJank Runner = do
      api <- ask
      let pool = filter (\c -> (sideCode c == Just "runner") && (not $ elem (fromMaybe "" $ packCode c) ["draft", "napd", "tdc"])) $ cards api
      identity <- liftIO $ chooseOne $ filter (\c -> typeCode c == Just "identity" && factionCode c /= Just "neutral") pool
      [c1, c2, c3] <- liftIO $ chooseN 3 $ filter (\c -> typeCode c /= Just "identity") pool
      return $ Jank identity c1 c2 c3
    embedJank :: Jank -> Message -> EnvDatabaseDiscord NrApi ()
    embedJank (Jank i c1 c2 c3) m = embedCards "Your jank combo is:" [i, c1, c2, c3] "" "" m
