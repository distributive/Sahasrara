-- |
-- Module      : Tablebot.Plugins.Netrunner.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of all Netrunner data in Tablebot.
module Tablebot.Plugins.Netrunner.Utility.NrApi (getNrApi) where

import Data.Aeson (FromJSON, Value (Object), eitherDecode, parseJSON, (.:))
import Data.Either (fromRight)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)
import Tablebot.Plugins.Netrunner.Type.BanList (BanList)
import Tablebot.Plugins.Netrunner.Type.Card (Card)
import Tablebot.Plugins.Netrunner.Type.Cycle (Cycle)
import Tablebot.Plugins.Netrunner.Type.Faction (Faction)
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Type.Pack (Pack)
import Tablebot.Plugins.Netrunner.Type.Type (Type)

-- | @getNrApi@ is a function that attempts to get the JSON objects containing
-- all required Netrunner data (cards, cycles, and packs) as provided by
-- https://netrunnerdb.com/api/2.0/doc.
getNrApi :: IO NrApi
getNrApi = do
  cardReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/cards"
  cardRes <- httpLBS cardReq
  let cardData = fromRight defaultCards ((eitherDecode $ responseBody cardRes) :: Either String Cards)
      cards = reverse $ cardContent cardData -- Reversing the list of cards prioritises newer cards in the search
      imageTemplate = imageUrlTemplate cardData
  cycleReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/cycles"
  cycleRes <- httpLBS cycleReq
  let cycleData = fromRight defaultCycles ((eitherDecode $ responseBody cycleRes) :: Either String Cycles)
      cycles = cycleContent cycleData
  factionReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/factions"
  factionRes <- httpLBS factionReq
  let factionData = fromRight defaultFactions ((eitherDecode $ responseBody factionRes) :: Either String Factions)
      factions = factionContent factionData
  packReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/packs"
  packRes <- httpLBS packReq
  let packData = fromRight defaultPacks ((eitherDecode $ responseBody packRes) :: Either String Packs)
      packs = packContent packData
  typeReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/types"
  typeRes <- httpLBS typeReq
  let typeData = fromRight defaultTypes ((eitherDecode $ responseBody typeRes) :: Either String Types)
      types = typeContent typeData
  banReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/mwl"
  banRes <- httpLBS banReq
  let banData = fromRight defaultBanLists ((eitherDecode $ responseBody banRes) :: Either String BanLists)
      banLists = banContent banData
  return NrApi {..}

-- | @Cards@ represents the full library of cards in Netrunner.
data Cards = Cards
  { cardContent :: ![Card],
    imageUrlTemplate :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Cards where
  parseJSON (Object v) = do
    content <- v .: "data"
    imageUrlTemplate <- v .: "imageUrlTemplate"
    return $ Cards {cardContent = content, imageUrlTemplate = imageUrlTemplate}
  parseJSON _ = return defaultCards

defaultCards :: Cards
defaultCards = Cards {cardContent = [], imageUrlTemplate = ""}

-- | @Cycles@ represents all cycles in the game's history.
-- This needs explaining: for some reason on NetrunnerDB the draft cycle
-- is listed as the 0th cycle (with the core set and sequential cycles 1
-- onwards. However, in the API draft isn't the 0th element in the list of
-- cycles. It's the *fourth* (zero indexed).
-- To avoid this weirdness propagating the order is fixed on import.
data Cycles = Cycles {cycleContent :: [Cycle]} deriving (Show, Generic)

instance FromJSON Cycles where
  parseJSON (Object v) = do
    content <- do
      cs <- v .: "data"
      return $ (cs !! 4) : (take 4 cs) ++ (drop 5 cs)
    return $ Cycles {cycleContent = content}
  parseJSON _ = return defaultCycles

defaultCycles :: Cycles
defaultCycles = Cycles {cycleContent = []}

-- | @Factions@ represents all factions in the game's.
data Factions = Factions {factionContent :: [Faction]} deriving (Show, Generic)

instance FromJSON Factions where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Factions {factionContent = content}
  parseJSON _ = return defaultFactions

defaultFactions :: Factions
defaultFactions = Factions {factionContent = []}

-- | @Packs@ represents all data packs in the game's history.
data Packs = Packs {packContent :: [Pack]} deriving (Show, Generic)

instance FromJSON Packs where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Packs {packContent = content}
  parseJSON _ = return defaultPacks

defaultPacks :: Packs
defaultPacks = Packs {packContent = []}

-- | @Types@ represents all card types in the game.
data Types = Types {typeContent :: [Type]} deriving (Show, Generic)

instance FromJSON Types where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ Types {typeContent = content}
  parseJSON _ = return defaultTypes

defaultTypes :: Types
defaultTypes = Types {typeContent = []}

-- | @BanLists@ represents all card types in the game.
data BanLists = BanLists {banContent :: [BanList]} deriving (Show, Generic)

instance FromJSON BanLists where
  parseJSON (Object v) = do
    content <- v .: "data"
    return $ BanLists {banContent = content}
  parseJSON _ = return defaultBanLists

defaultBanLists :: BanLists
defaultBanLists = BanLists {banContent = []}
