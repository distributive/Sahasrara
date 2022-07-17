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
import Data.Text (Text, intercalate)
import Discord.Types
import qualified Sahasrara.Plugins.Netrunner.Type.Cycle as C
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Pack (Pack (cycleCode, name))
import qualified Sahasrara.Plugins.Netrunner.Type.Pack as P
import Sahasrara.Plugins.Netrunner.Utility.Embed (embedText)
import Sahasrara.Utility hiding (name)
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
      legalPacks = name <$> snd ram
      codes = intercalate "|" $ map P.code $ (fst ram) ++ (snd ram)
  sendEmbedMessage m "" $
    addColour DiscordColorGreen $
      embedText ":game_die: RAM :game_die:" $
        (if length bigBoxes > 0 then "**Large Releases**\n" <> intercalate "\n" bigBoxes <> "\n\n" else "")
          <> (if length legalPacks > 0 then "**Data Packs**\n" <> intercalate "\n" legalPacks <> "\n\n" else "")
          <> "**NetrunnerDB link**\nSee the list of legal cards for this card pool [here](https://netrunnerdb.com/find/?q=e:"
          <> codes
          <> ")!"

-- | @generateRam@ generates a card pool with a set number of data packs and
-- core/deluxe expansions.
generateRam :: NrApi -> Int -> Int -> IO ([Pack], [Pack])
generateRam NrApi {cycles = cycles, packs = packs} bigBoxCount packCount = do
  a <- chooseN bigBoxCount bigBoxes
  b <- chooseN packCount legalPacks
  return (a, b)
  where
    bigBoxes :: [Pack]
    bigBoxes = withCode ["core", "cac", "hap", "oac", "dad", "td", "core", "rar", "sg"]
    legalPacks :: [Pack]
    legalPacks =
      let legalCycles = filter (\c -> (length $ filter (\p -> C.code c == cycleCode p) packs) > 2) cycles
          dataPacks = concatMap (\c -> filter (\p -> C.code c == cycleCode p) packs) legalCycles
          extraPacks = withCode ["sm", "mor"]
          removedPacks = withCode ["sc19", "su21", "urbp", "dag", "bm"]
       in filter (not . (flip elem removedPacks)) $ dataPacks ++ extraPacks
    withCode :: [Text] -> [Pack]
    withCode xs = filter (\p -> elem (P.code p) xs) packs
