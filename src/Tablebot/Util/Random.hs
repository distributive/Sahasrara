-- |
-- Module      : Tablebot.Util.Random
-- Description : A plugin for random generation.
-- Copyright   : (c) Amelie WD 2021
-- License     : MIT
-- Maintainer  : tablebot@ameliewd.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A collection of utility functions for generating randomness.
module Tablebot.Util.Random (chooseOne, chooseOneWithDefault, chooseOneWeighted, chooseOneWeightedWithDefault) where

import Data.List
import Data.Maybe
import System.Random (randomRIO)
import Tablebot.Plugin.Error

-- | @chooseOne@ chooses a single random element from a given list with uniform
-- distribution.
chooseOne :: [a] -> IO (Either Error a)
chooseOne [] = return $ Left "Cannot choose from empty list."
chooseOne xs = Right . (xs !!) <$> randomRIO (0, length xs - 1 :: Int)

-- | @chooseOneWithDefault@ chooses a single random element from a given list
-- with uniform distribution, or a given default value if the list is empty.
chooseOneWithDefault :: a -> [a] -> IO a
chooseOneWithDefault x xs = either (const x) id <$> (chooseOne xs)

-- | @chooseOneWeighted@ chooses a single random element from a given list with
-- weighted distribution as defined by a given weighting function.
-- The function works by zipping each element with its cumulative weight, then
-- choosing a random element indexed by [0, totalWeight)
chooseOneWeighted :: (a -> Int) -> [a] -> IO (Either Error a)
chooseOneWeighted _ [] = return $ Left "Cannot choose from empty list."
chooseOneWeighted weight xs
  | any ((< 0) . weight) xs = return $ Left "Probability weightings cannot be negative."
  | all ((== 0) . weight) xs = return $ Left "At least one weighting must be positive."
  | otherwise =
    Right . fst . fromJust
      . (\i -> find ((> i) . snd) (zip xs' $ scanl1 (+) $ weight <$> xs'))
      <$> randomRIO (0, totalWeight - 1)
  where
    xs' = filter ((> 0) . weight) xs -- removes elements with a weight of zero
    totalWeight = sum $ weight <$> xs'

-- | @chooseOneWeightedWithDefault@ chooses a single random element from a given
-- list with weighted distribution as defined by a given weighting function, or
-- a given default if the list is empty
chooseOneWeightedWithDefault :: a -> (a -> Int) -> [a] -> IO a
chooseOneWeightedWithDefault x weight xs = either (const x) id <$> (chooseOneWeighted weight xs)
