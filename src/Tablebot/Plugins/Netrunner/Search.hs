{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Tablebot.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions for embedding Netrunner data.
module Tablebot.Plugins.Netrunner.Search
  ( queryCard,
    searchCards,
    fixSearch,
    pairsToQuery,
    pairsToNrdb,
  )
where

import Data.Bifunctor (first)
import Data.List (nubBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, intercalate, isInfixOf, pack, replace, singleton, unpack, unwords)
import Tablebot.Plugins.Netrunner.Type.Card as Card (Card (..))
import Tablebot.Plugins.Netrunner.Type.Faction as Faction (Faction (..))
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Utility.Search (FuzzyCosts (..), autocomplete, closestMatch, closestValueWithCosts)
import Tablebot.Utility.Types ()
import Tablebot.Utility.Utils (standardise)
import Text.Read (readMaybe)
import Prelude hiding (unwords)

-- | @queryCard@ searches the given library of cards by title, first checking if
-- the search query is a substring of any cards, then performing a fuzzy search on
-- the cards given, or all of the cards if no cards are found
queryCard :: NrApi -> Text -> Card
queryCard NrApi {cards = cards} txt = findCard (substringSearch pairs txt) txt pairs
  where
    pairs = zip (map (standardise . fromMaybe "" . Card.title) cards) cards
    substringSearch pairs' searchTxt = filter (isInfixOf (standardise searchTxt) . fst) pairs'

-- | @findCard@ finds a card from the given list of pairs that is some subset of a
-- full list. If the sublist is empty, it will fuzzy search the full list. If the sublist
-- has exactly 1 element, it'll return that element. If the sublist has multiple
-- elements, it will fuzzy search the sublist
findCard :: [(Text, Card)] -> Text -> [(Text, Card)] -> Card
findCard [] searchTxt allCards = fuzzyQueryCard allCards searchTxt
findCard [(_, card)] _ _ = card
findCard cards searchTxt _ = fuzzyQueryCard cards searchTxt

-- | @queryCard@ fuzzy searches the given library of cards by title.
fuzzyQueryCard :: [(Text, Card)] -> Text -> Card
fuzzyQueryCard pairs = closestValueWithCosts editCosts unpackedPairs . unpack
  where
    unpackedPairs = fmap (first unpack) pairs
    editCosts =
      FuzzyCosts
        { deletion = 10,
          insertion = 2,
          substitution = 10,
          transposition = 1
        }

-- | @searchCards@ looks for all cards that match a set of criteria.
-- TODO: ensure in fixSearch that invalid queries are pruned before reaching this function
searchCards :: NrApi -> [(String, Char, [String])] -> Maybe [Card]
searchCards _ [] = Nothing
searchCards NrApi {cards = cards} pairs = Just $ nubBy cardEq $ foldr filterCards cards $ packValues pairs
  where
    packValues :: [(String, Char, [String])] -> [(String, Char, [Text])]
    packValues = map (\(k, sep, v) -> (k, sep, map pack v))
    cardEq :: Card -> Card -> Bool
    cardEq a b = title a == title b
    filterCards :: (String, Char, [Text]) -> [Card] -> [Card]
    filterCards ("x", sep, xs) = filterText sep stripped_text xs
    filterCards ("a", sep, xs) = filterText sep flavor xs
    filterCards ("e", sep, xs) = filterText sep pack_code xs
    -- filterCards ("c", sep, xs) =
    filterCards ("t", sep, xs) = filterText sep type_code xs
    filterCards ("f", sep, xs) = filterText sep faction_code xs
    filterCards ("s", sep, xs) = filterText sep keywords xs
    filterCards ("d", sep, xs) = filterText sep side_code xs
    filterCards ("i", sep, xs) = filterText sep illustrator xs
    filterCards ("o", sep, xs) = filterInt sep cost xs
    filterCards ("g", sep, xs) = filterInt sep advancement_cost xs
    filterCards ("m", sep, xs) = filterInt sep memory_cost xs
    filterCards ("n", sep, xs) = filterInt sep faction_cost xs
    filterCards ("p", sep, xs) = filterInt sep strength xs
    filterCards ("v", sep, xs) = filterInt sep agenda_points xs
    filterCards ("h", sep, xs) = filterInt sep trash_cost xs
    -- filterCards ("r", sep, xs) cs =
    filterCards ("u", sep, xs) = filterBool sep uniqueness xs
    -- filterCards ("b", sep, xs) cs =
    -- filterCards ("z", sep, xs) cs =
    filterCards _ = id
    filterText :: Char -> (Card -> Maybe Text) -> [Text] -> ([Card] -> [Card])
    filterText sep f xs =
      let check c x = isInfixOf (standardise x) $ standardise $ fromMaybe "" $ f c
          fil c = any (check c) xs
       in case sep of
            ':' -> filter fil
            '!' -> filter (not . fil)
            _ -> id
    filterInt :: Char -> (Card -> Maybe Int) -> [Text] -> ([Card] -> [Card])
    filterInt sep f xs = case concat $ mapMaybe (readMaybe . unpack) xs of
      [] -> id
      xs' -> filter (maybe False (\y -> any (sep `toEquality` y) xs') . f)
    toEquality :: Char -> (Int -> Int -> Bool)
    toEquality ':' = (==)
    toEquality '!' = (/=)
    toEquality '>' = (<)
    toEquality '<' = (>)
    toEquality _ = \_ _ -> True
    filterBool :: Char -> (Card -> Maybe Bool) -> a -> ([Card] -> [Card])
    filterBool ':' f _ = filter (fromMaybe False . f)
    filterBool '!' f _ = filter (maybe True not . f)
    filterBool _ _ _ = id

-- | @fixSearch@ takes a set of key/value pairs and repairs damaged queries to
-- make them valid queries for NetrunnerDB.
fixSearch :: NrApi -> [(String, Char, [String])] -> [(String, Char, [String])]
fixSearch NrApi {factions = factions} = map fix
  where
    fix ("f", sep, fs) = ("f", sep, map fixFaction fs)
    fix p = p
    fixFaction f = case autocomplete fNames $ pack f of
      Just f' -> unpack f'
      Nothing -> closestMatch (map unpack fNames) f
    fNames = map Faction.code factions

-- | @pairsToQuery@ takes a set of search query pairs ands turns it into a link
-- to an equivalent search on NetrunnerDB.
pairsToQuery :: [(String, Char, [String])] -> Text
pairsToQuery pairs = "<https://netrunnerdb.com/find/?q=" <> replace " " "+" (pairsToNrdb pairs) <> ">"

-- | @pairsToNrdb@ takes a set of search query pairs and formats it into a valid
-- plaintext search query for NetrunnerDB.
pairsToNrdb :: [(String, Char, [String])] -> Text
pairsToNrdb pairs = unwords queries
  where
    queries = map format pairs
    format (k, sep, vs) =
      let v = intercalate "|" $ map (formatValue . pack) vs
       in pack k <> singleton sep <> v
    formatValue v =
      if " " `isInfixOf` v
        then "\"" <> v <> "\""
        else v
