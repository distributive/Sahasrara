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
import Data.List (nub)
import Data.Text (intercalate, pack, toLower, unpack)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Card (Card (cardTypeCode, factionCode, sideCode))
import Sahasrara.Plugins.Netrunner.Type.Format (Format)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Format (getEternal, getStandard, getStartup, toActiveSnapshot)
import Sahasrara.Plugins.Netrunner.Utility.Print (embedCards)
import Sahasrara.Plugins.Netrunner.Utility.Snapshot
import Sahasrara.Utility
import Sahasrara.Utility.Exception (BotException (GenericException), throwBot)
import Sahasrara.Utility.Parser
import Sahasrara.Utility.Random (chooseN, chooseOne)
import Text.Megaparsec

data Side = Corp | Runner deriving (Show, Eq)

data Jank = Jank Card Card Card Card

-- | @nrJank@ is a command randomly generating an ID and three non-ID cards.
nrJank :: EnvCommand NrApi
nrJank = Command "jank" jankComm []
  where
    jankComm :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    jankComm = do
      args <- (try quoted <|> nonSpaceWord) `sepBy` some space
      return $ \m -> do
        api <- ask
        let args' = nub $ map (toLower . pack) args
            sides = [("corp", Corp), ("runner", Runner)]
            formats = [("standard", getStandard api), ("startup", getStartup api), ("eternal", getEternal api)]
            sArgs = filter ((`elem` args') . fst) sides
            fArgs = filter ((`elem` args') . fst) formats
            unknowns = filter (not . (`elem` ((map fst formats) ++ (map fst sides)))) args'
        side <- case sArgs of
          [] -> liftIO $ chooseOne [Corp, Runner]
          [s] -> return $ snd s
          _ -> throwBot $ GenericException "Multiple sides provided" "Please only specify one side"
        format <- case fArgs of
          [] -> return $ getStandard api
          [f] -> return $ snd f
          _ -> throwBot $ GenericException "Multiple formats provided" "Please only specify one format"
        jank <- generateJank side format
        if length unknowns == 0
          then embedJank jank m
          else
            throwBot $
              GenericException
                "Unrecognised arguments"
                $ "Could not parse "
                  ++ (unpack $ intercalate ", " unknowns)
                  ++ "\nPlease provide up to one side (`corp` or `runner`) and up to one valid format (`standard`, `startup`, or `eternal`)"
    getPool :: NrApi -> Side -> Format -> [Card]
    getPool api side format =
      let legalCards = toLegalCards api $ toActiveSnapshot api format
       in case side of
            Corp -> filter (("corp" ==) . sideCode) legalCards
            Runner -> filter (("runner" ==) . sideCode) legalCards
    generateJank :: Side -> Format -> EnvDatabaseDiscord NrApi Jank
    generateJank side format = do
      api <- ask
      let pool = getPool api side format
          idCode = if side == Corp then "corp_identity" else "runner_identity"
      identity <- liftIO $ chooseOne $ filter ((idCode ==) . cardTypeCode) pool
      [c1, c2, c3] <- liftIO $ chooseN 3 $ filter (\c -> (cardTypeCode c /= "agenda" || factionCode c == factionCode identity) && cardTypeCode c /= idCode) pool
      return $ Jank identity c1 c2 c3
    embedJank :: Jank -> Message -> EnvDatabaseDiscord NrApi ()
    embedJank (Jank i c1 c2 c3) m = embedCards "Your jank combo is:" "" [i, c1, c2, c3] "" "" m
