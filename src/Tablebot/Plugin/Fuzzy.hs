-- |
-- Module      : Tablebot.Plugin.Fuzzy
-- Description : A custom fuzzy search module.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Implements some functions for fuzzy matching strings.
module Tablebot.Plugin.Fuzzy
  ( FuzzyCosts (..),
    closestMatch,
    closestMatchWithCosts,
    closestPair,
    closestPairWithCosts,
    closestValue,
    closestValueWithCosts
  ) where

import Data.Char (toLower)
import Data.List (minimumBy)
import Text.EditDistance

-- | @FuzzyCosts@ is a wrapper for Text.EditDistance's EditCosts data type for
-- easier interfacing.
data FuzzyCosts = FuzzyCosts
  { deletion :: Int,
    insertion :: Int,
    substitution :: Int,
    transposition :: Int
  }

-- | @convertCosts@ turns the custom FuzzyCosts into Text.EditDistance's
-- EditCosts.
convertCosts :: FuzzyCosts -> EditCosts
convertCosts costs = EditCosts
  { deletionCosts = ConstantCost $ deletion costs,
    insertionCosts = ConstantCost $ insertion costs,
    substitutionCosts = ConstantCost $ substitution costs,
    transpositionCosts = ConstantCost $ transposition costs
  }

-- | @defaultFuzzyCosts@ mirrors the values of Text.EditDistance's
-- defaultEditCosts.
defaultFuzzyCosts :: FuzzyCosts
defaultFuzzyCosts = FuzzyCosts
  { deletion = 1,
    insertion = 1,
    substitution = 1,
    transposition = 1
  }

-- | @closestMatch@ takes a set of strings and a query and finds the string that
-- most closely matches the query.
closestMatch :: [String] -> String -> String
closestMatch = closestMatchWithCosts defaultFuzzyCosts

-- | @closestMatchWithCosts@ is @closestMatch@ with customisable edit costs.
closestMatchWithCosts :: FuzzyCosts -> [String] -> String -> String
closestMatchWithCosts editCosts strings query = minimumBy comparison strings
  where
    score :: String -> Int
    score = levenshteinDistance (convertCosts editCosts) (map toLower query)
    comparison :: String -> String -> Ordering
    comparison a b
      | score a < score b = LT
      | score a > score b = GT
      | otherwise = EQ

-- | @closestPair@ takes a set of pairs and a query and finds the pair whose key
-- most closely matches the query.
closestPair :: [(String, a)] -> String -> (String, a)
closestPair = closestPairWithCosts defaultFuzzyCosts

-- | @closestPairWithCosts@ is @closestPair@ with customisable edit costs.
closestPairWithCosts :: FuzzyCosts -> [(String, a)] -> String -> (String, a)
closestPairWithCosts editCosts pairs query = minimumBy comparison pairs
  where
    score :: String -> Int
    score = levenshteinDistance (convertCosts editCosts) (map toLower query)
    comparison :: (String, a) -> (String, a) -> Ordering
    comparison (a,_) (b,_)
      | score a < score b = LT
      | score a > score b = GT
      | otherwise = EQ

-- | @closestValue@ is @closestPair@ but it only returns the value of the
-- matched pair.
closestValue :: [(String, a)] -> String -> a
closestValue = closestValueWithCosts defaultFuzzyCosts

-- | @closestValueWithCosts@ is @closestValue@ with customisable edit costs.
closestValueWithCosts :: FuzzyCosts -> [(String, a)] -> String -> a
closestValueWithCosts editCosts pairs query = snd $ closestPairWithCosts editCosts pairs query
