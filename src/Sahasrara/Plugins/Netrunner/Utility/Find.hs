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
module Sahasrara.Plugins.Netrunner.Utility.Find (queryCard) where

import Data.Text (Text, isInfixOf, unpack)
import Sahasrara.Plugins.Netrunner.Type.Card as Card (Card (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Alias (fromAlias)
import Sahasrara.Utility
import Sahasrara.Utility.Search (FuzzyCosts (..), closestValueWithCosts)
import Sahasrara.Utility.Types ()

-- | @queryCard@ searches the given library of cards by title, first checking if
-- the search query is a substring of any cards (prioritising cards it matches
-- the start of), then performing a fuzzy search on the cards given, or all of
-- the cards if no cards are found
-- If the given query matches an alias, it will first dereference that alias
queryCard :: NrApi -> Text -> Card
queryCard NrApi {cards = cards, cardAliases = cardAliases} query =
  let q = standardise $ fromAlias cardAliases $ standardise query
   in case filter (isInfixOf (standardise q) . fst) pairs of
        [] -> fuzzyQueryCard pairs q
        [res] -> snd res
        res -> fuzzyQueryCard res q
  where
    pairs :: [(Text, Card)]
    pairs = zip (map (standardise . Card.title) cards) cards
    fuzzyQueryCard :: [(Text, Card)] -> Text -> Card
    fuzzyQueryCard pairs' =
      let unpackedPairs = fmap (\(x, y) -> (unpack x, y)) pairs'
       in closestValueWithCosts editCosts unpackedPairs . unpack
    editCosts :: FuzzyCosts
    editCosts =
      FuzzyCosts
        { deletion = 10,
          insertion = 2,
          substitution = 10,
          transposition = 1
        }
