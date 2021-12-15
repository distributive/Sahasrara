-- |
-- Module      : Tablebot.Plugin.Netrunner.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of all Netrunner data in Tablebot.
module Tablebot.Plugin.Netrunner.NrApi (NrApi (..), getNrApi) where

import Data.Aeson (eitherDecode)
import Data.Either (fromRight)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)
import Tablebot.Plugin.Netrunner.Card as Card
import Tablebot.Plugin.Netrunner.Cycle as Cycle
import Tablebot.Plugin.Netrunner.Faction as Faction
import Tablebot.Plugin.Netrunner.Pack as Pack

-- | @NrApi@ represents all required Netrunner data collected.
data NrApi = NrApi
  { cards :: [Card],
    cycles :: [Cycle],
    factions :: [Faction],
    packs :: [Pack],
    imageTemplate :: Text
  }
  deriving (Show, Generic)

-- | @getNrApi@ is a function that attempts to get the JSON objects containing
-- all required Netrunner data (cards, cycles, and packs) as provided by
-- https://netrunnerdb.com/api/2.0/doc.
getNrApi :: IO NrApi
getNrApi = do
  cardReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/cards"
  cardRes <- httpLBS cardReq
  let cardData = fromRight defaultCards ((eitherDecode $ responseBody cardRes) :: Either String Cards)
  cycleReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/cycles"
  cycleRes <- httpLBS cycleReq
  let cycleData = fromRight defaultCycles ((eitherDecode $ responseBody cycleRes) :: Either String Cycles)
  factionReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/factions"
  factionRes <- httpLBS factionReq
  let factionData = fromRight defaultFactions ((eitherDecode $ responseBody factionRes) :: Either String Factions)
  packReq <- parseRequest "https://netrunnerdb.com/api/2.0/public/packs"
  packRes <- httpLBS packReq
  let packData = fromRight defaultPacks ((eitherDecode $ responseBody packRes) :: Either String Packs)
  return $
    NrApi
      { cards = reverse $ Card.content cardData, -- Reversing the list of cards prioritises newer cards in the search
        cycles = Cycle.content cycleData,
        factions = Faction.content factionData,
        packs = Pack.content packData,
        imageTemplate = Card.imageUrlTemplate cardData
      }
