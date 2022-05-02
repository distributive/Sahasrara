-- |
-- Module      : Sahasrara.Plugins.Netrunner.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of all Netrunner data in Sahasrara.
module Sahasrara.Plugins.ONR.Utility.OnrApi (getOnrApi) where

import Data.Aeson (FromJSON, Value (Object), eitherDecode, parseJSON, (.:))
import Data.Either (fromRight)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)
import Sahasrara.Plugins.ONR.Type.Card (Card)
import Sahasrara.Plugins.ONR.Type.OnrApi (OnrApi (..))

-- | @getNrApi@ is a function that attempts to get the JSON objects containing
-- all required ONR data.
getOnrApi :: IO OnrApi
getOnrApi = do
  cardReq <- parseRequest "https://distributive.github.io/onr-cards-json/cards.json"
  cardRes <- httpLBS cardReq
  let cardData = fromRight defaultCards ((eitherDecode $ responseBody cardRes) :: Either String Cards)
      cards = cardContent cardData
      imageTemplate = imageUrlTemplate cardData
  return OnrApi {..}

-- | @Cards@ represents the full library of cards in Netrunner.
data Cards = Cards
  { cardContent :: ![Card],
    imageUrlTemplate :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Cards where
  parseJSON (Object v) = do
    content <- v .: "cards"
    imageUrlTemplate <- v .: "imageUrlTemplate"
    return $ Cards {cardContent = content, imageUrlTemplate = imageUrlTemplate}
  parseJSON _ = return defaultCards

defaultCards :: Cards
defaultCards = Cards {cardContent = [], imageUrlTemplate = ""}
