{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions for finding cards in the api.
module Sahasrara.Plugins.ONR.Utility.Find (queryCard) where

import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isInfixOf, unpack)
import qualified Data.Text as T
import Sahasrara.Plugins.ONR.Type.Card as Card (Card (..))
import Sahasrara.Plugins.ONR.Type.OnrApi (OnrApi (..))
import Sahasrara.Utility.Search (FuzzyCosts (..), closestValueWithCosts)
import Sahasrara.Utility.Utils (standardise)

-- | @queryCard@ searches the given library of cards by title, first checking if
-- the search query is a substring of any cards (prioritising cards it matches
-- the start of), then performing a fuzzy search on the cards given, or all of
-- the cards if no cards are found
-- If the given query matches an alias, it will first dereference that alias
queryCard :: OnrApi -> Text -> Card
queryCard OnrApi {cards = cards} txt =
  let q = standardise txt --fromAlias cardAliases $ standardise txt
   in findCard (substringSearch pairs q) q pairs
  where
    pairs = zip (map (standardise . fromMaybe "" . Card.title) cards) cards
    substringSearch pairs' searchTxt =
      let pres = filter (isPrefixOf searchTxt . fst) pairs'
          subs = filter (isInfixOf searchTxt . fst) pairs'
       in case length pres of
            0 -> subs
            _ -> pres
    isPrefixOf :: Text -> Text -> Bool
    isPrefixOf short long = short == T.take (T.length short) long

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
