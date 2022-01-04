-- |
-- Module      : Tablebot.Plugins.Netrunner.Custom
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the custom command.
module Tablebot.Plugins.Netrunner.Command.Custom (customCard) where

import Data.Map (fromList, lookup)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Tablebot.Plugins.Netrunner.Type.Card as Card
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Utility.Search (FuzzyCosts (..), closestPairWithCosts, closestValue)
import Prelude hiding (lookup)

-- | @NrData@ allows us to parse parameters into a type of our choosing.
data NrData = NrText Text | NrInt Int | NrBool Bool

-- | @nrToText@ attempts to get Text from a NrData.
nrToText :: NrData -> Maybe Text
nrToText (NrText t) = Just t
nrToText _ = Nothing

-- | @nrToText@ attempts to get an Int from a NrData.
nrToInt :: NrData -> Maybe Int
nrToInt (NrInt x) = Just x
nrToInt _ = Nothing

-- | @nrToText@ attempts to get a Bool from a NrData.
nrToBool :: NrData -> Maybe Bool
nrToBool (NrBool b) = Just b
nrToBool _ = Nothing

-- | @keys@ maps accepted card record keys to their parser functions.
keys :: [(String, Maybe String -> Maybe NrData)]
keys =
  [ ("advancement", fmap $ NrInt . readInt),
    ("points", fmap $ NrInt . readInt),
    ("link", fmap $ NrInt . readInt),
    ("cost", fmap $ NrInt . readInt),
    ("faction", fmap $ NrText . pack),
    ("influence", fmap $ NrInt . readInt),
    ("flavour", fmap $ NrText . pack),
    ("maxInf", fmap $ NrInt . readInt),
    ("keywords", fmap $ NrText . pack),
    ("mu", fmap $ NrInt . readInt),
    ("minSize", fmap $ NrInt . readInt),
    ("strength", fmap $ NrInt . readInt),
    ("text", fmap $ NrText . pack),
    ("title", fmap $ NrText . pack),
    ("trash", fmap $ NrInt . readInt),
    ("type", fmap $ NrText . pack),
    ("unique", fmap $ NrBool . fuzzyBool)
  ]
  where
    readInt :: String -> Int
    readInt = read

-- | @fuzzyBool@ maps a string to true or false (whichever is closest).
fuzzyBool :: String -> Bool
fuzzyBool = closestValue [("true", True), ("false", False)]

-- -- | @fuzzyType@ maps a string to the closest card type.
-- fuzzyType :: String -> String
-- fuzzyType = closestMatch ["agenda", "asset", "event", "hardware", "ice", "identity", "operation", "program", "resource", "upgrade"]
--
-- -- | @fuzzyFaction@ maps a string to the closest card faction by name.
-- fuzzyFaction :: NrApi -> String -> String
-- fuzzyFaction api query =
--   let names = unpack . Faction.name <$> factions api
--       codes = unpack . Faction.code <$> factions api
--    in closestValue (zip names codes) query

-- | @customCard@ takes a set of parameters and does its best to turn that data
-- into a card.
customCard :: NrApi -> [(String, String)] -> Card
customCard _ cardData =
  let params = fromList $ catMaybes $ readParam <$> cardData
   in Card
        { advancementCost = nrToInt =<< lookup "advancement" params,
          agendaPoints = nrToInt =<< lookup "points" params,
          baseLink = nrToInt =<< lookup "link" params,
          Card.code = Nothing,
          cost = nrToInt =<< lookup "cost" params,
          deckLimit = Nothing,
          factionCode = nrToText =<< lookup "faction" params,
          factionCost = nrToInt =<< lookup "influence" params,
          flavour = nrToText =<< lookup "flavour" params,
          illustrator = Nothing,
          influenceLimit = nrToInt =<< lookup "maxInf" params,
          keywords = nrToText =<< lookup "keywords" params,
          memoryCost = nrToInt =<< lookup "mu" params,
          minimumDeckSize = nrToInt =<< lookup "minSize" params,
          packCode = Nothing,
          position = Nothing,
          quantity = Nothing,
          Card.sideCode = Nothing,
          strength = nrToInt =<< lookup "strength" params,
          strippedText = nrToText =<< lookup "text" params, -- TODO - strip this
          strippedTitle = nrToText =<< lookup "title" params, -- TODO - ditto
          text = nrToText =<< lookup "text" params,
          title = nrToText =<< lookup "title" params,
          trashCost = nrToInt =<< lookup "trash" params,
          typeCode = nrToText =<< lookup "type" params,
          uniqueness = nrToBool =<< lookup "unique" params
        }
  where
    readParam :: (String, String) -> Maybe (String, NrData)
    readParam (k, v) =
      let (field, parser) = closestPairWithCosts costs keys k
       in case parser (Just v) of
            Just value -> Just (field, value)
            Nothing -> Nothing
    costs :: FuzzyCosts
    costs =
      FuzzyCosts
        { deletion = 10,
          insertion = 1,
          substitution = 10,
          transposition = 10
        }
