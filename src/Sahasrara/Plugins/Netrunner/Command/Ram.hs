{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for commands generating RAM card pools.
module Sahasrara.Plugins.Netrunner.Command.Ram (nrRam) where

import Control.Monad.Reader (ask, liftIO)
import Data.Text (intercalate)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.CardSet (CardSet (cardSetTypeCode, name))
import qualified Sahasrara.Plugins.Netrunner.Type.CardSet as CardSet
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Embed (embedText)
import Sahasrara.Utility
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (sendEmbedMessage)
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Exception (BotException (GenericException), throwBot)
import Sahasrara.Utility.Random (chooseN)
import Sahasrara.Utility.SmartParser (NonNegativeInt (NonNegativeInt), PComm (parseComm), WithError (WErr))
import Sahasrara.Utility.Types ()

-- | @nrRam@ is a command that generates a RAM card pool.
nrRam :: EnvCommand NrApi
nrRam = Command "ram" (parseComm ramComm) []
  where
    ramComm ::
      WithError
        "Expected two non-negative integers"
        (Either () (NonNegativeInt, NonNegativeInt)) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    ramComm (WErr (Left ())) = embedRam 2 12
    ramComm (WErr (Right (NonNegativeInt a, NonNegativeInt b))) = embedRam a b

-- | @embedRam@ embeds a RAM card pool.
embedRam :: Int -> Int -> Message -> EnvDatabaseDiscord NrApi ()
embedRam 0 0 _ = throwBot $ GenericException "No sets requested!" "Please request at least one large expansion or data pack."
embedRam a b m = do
  api <- ask
  ram <- liftIO $ generateRam api a b
  let bigBoxes = name <$> fst ram
      legalSets = name <$> snd ram
      codes = intercalate "|" $ map CardSet.legacyCode $ (fst ram) ++ (snd ram)
  sendEmbedMessage m "" $
    addColour colInfo $
      embedText ":game_die: RAM :game_die:" $
        (if length bigBoxes > 0 then "**Large Releases**\n" <> intercalate "\n" bigBoxes <> "\n\n" else "")
          <> (if length legalSets > 0 then "**Data Packs**\n" <> intercalate "\n" legalSets <> "\n\n" else "")
          <> "**NetrunnerDB link**\nSee the list of legal cards for this card pool [here](https://netrunnerdb.com/find/?q=e:"
          <> codes
          <> ")!"

-- | @generateRam@ generates a card pool with a set number of card sets and
-- core/deluxe expansions.
generateRam :: NrApi -> Int -> Int -> IO ([CardSet], [CardSet])
generateRam NrApi {cardSets = cardSets} largeCount smallCount = do
  large <- chooseN largeCount $ filter (\s -> cardSetTypeCode s `elem` ["core", "deluxe"] && (not $ name s `elem` ["System Core 2019", "System Update 2021"])) cardSets
  small <- chooseN smallCount $ filter (\s -> cardSetTypeCode s `elem` ["data_pack", "expansion"] && (not $ name s `elem` ["Magnum Opus", "Democracy and Dogma", "Blood Money"])) cardSets
  return (large, small)
