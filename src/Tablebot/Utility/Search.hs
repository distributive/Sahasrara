-- |
-- Module      : Tablebot.Utility.Search
-- Description : A custom fuzzy search module.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Implements some functions for fuzzy matching strings.
module Tablebot.Utility.Search
  ( FuzzyCosts (..),
    closestMatch,
    closestMatchWithCosts,
    closestPair,
    closestPairWithCosts,
    closestValue,
    closestValueWithCosts,
    shortestSuperString,
    autocomplete,
  )
where

import Data.Char (toLower)
import Data.List (minimumBy)
import Data.Text (Text, isInfixOf, length, take)
import Text.EditDistance

-- | @compareOn@ is a helper function for comparing types that aren't ord.
compareOn :: Ord b => (a -> b) -> a -> a -> Ordering
compareOn comp a b = compare (comp a) (comp b)

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
convertCosts costs =
  EditCosts
    { deletionCosts = ConstantCost $ deletion costs,
      insertionCosts = ConstantCost $ insertion costs,
      substitutionCosts = ConstantCost $ substitution costs,
      transpositionCosts = ConstantCost $ transposition costs
    }

-- | @defaultFuzzyCosts@ mirrors the values of Text.EditDistance's
-- defaultEditCosts.
defaultFuzzyCosts :: FuzzyCosts
defaultFuzzyCosts =
  FuzzyCosts
    { deletion = 1,
      insertion = 1,
      substitution = 1,
      transposition = 1
    }

-- | @closestMatch@ takes a list of strings and a query and finds the string
-- that most closely matches the query.
closestMatch :: [String] -> String -> String
closestMatch = closestMatchWithCosts defaultFuzzyCosts

-- | @closestMatchWithCosts@ is @closestMatch@ with customisable edit costs.
closestMatchWithCosts :: FuzzyCosts -> [String] -> String -> String
closestMatchWithCosts editCosts strings query = minimumBy (compareOn score) strings
  where
    score :: String -> Int
    score = levenshteinDistance (convertCosts editCosts) (map toLower query)

-- | @closestPair@ takes a set of pairs and a query and finds the pair whose key
-- most closely matches the query.
closestPair :: [(String, a)] -> String -> (String, a)
closestPair = closestPairWithCosts defaultFuzzyCosts

-- | @closestPairWithCosts@ is @closestPair@ with customisable edit costs.
closestPairWithCosts :: FuzzyCosts -> [(String, a)] -> String -> (String, a)
closestPairWithCosts editCosts pairs query = minimumBy (compareOn $ score . fst) pairs
  where
    score :: String -> Int
    score = levenshteinDistance (convertCosts editCosts) (map toLower query)

-- | @closestValue@ is @closestPair@ but it only returns the value of the
-- matched pair.
closestValue :: [(String, a)] -> String -> a
closestValue = closestValueWithCosts defaultFuzzyCosts

-- | @closestValueWithCosts@ is @closestValue@ with customisable edit costs.
closestValueWithCosts :: FuzzyCosts -> [(String, a)] -> String -> a
closestValueWithCosts editCosts pairs query = snd $ closestPairWithCosts editCosts pairs query

-- | @shortestSuperString@ takes a list of strings and matches the shortest one
-- that contains the given query as a strict substring.
-- Note that if a string in the list is a superstring of another element of the
-- list it cannot be matched.
shortestSuperString :: [Text] -> Text -> Maybe Text
shortestSuperString ts query = case filter (query `isInfixOf`) ts of
  [] -> Nothing
  xs -> Just $ minimumBy (compareOn Data.Text.length) xs

-- | @autocomplete@ is @shortestSuperString@ except the query must be a
-- substring starting from the first character of the matched text.
autocomplete :: [Text] -> Text -> Maybe Text
autocomplete ts query = case filter ((== query) . Data.Text.take (Data.Text.length query)) ts of
  [] -> Nothing
  xs -> Just $ minimumBy (compareOn Data.Text.length) xs
