-- |
-- Module      : Tablebot.Utility.Random
-- Description : A plugin for random generation.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A collection of utility functions for generating randomness.
module Tablebot.Utility.Random (chooseOne, chooseOneWithDefault, chooseOneWeighted, chooseOneWeightedWithDefault) where

import Control.Monad.Exception (MonadException (throw))
import Data.List (find)
import Data.Maybe (fromJust)
import System.Random (randomRIO)
import Tablebot.Utility.Exception (BotException (RandomException), catchBot)

-- | @chooseOne@ chooses a single random element from a given list with uniform
-- distribution.
chooseOne :: [a] -> IO a
chooseOne [] = throw $ RandomException "Cannot choose from empty list."
chooseOne xs = (xs !!) <$> randomRIO (0, length xs - 1 :: Int)

-- | @chooseOneWithDefault@ chooses a single random element from a given list
-- with uniform distribution, or a given default value if the list is empty.
chooseOneWithDefault :: a -> [a] -> IO a
chooseOneWithDefault x xs = chooseOne xs `catchBot` \_ -> return x

-- | @chooseOneWeighted@ chooses a single random element from a given list with
-- weighted distribution as defined by a given weighting function.
-- The function works by zipping each element with its cumulative weight, then
-- choosing a random element indexed by a random value in [0, totalWeight)
chooseOneWeighted :: (a -> Int) -> [a] -> IO a
chooseOneWeighted _ [] = throw $ RandomException "Cannot choose from empty list."
chooseOneWeighted weight xs
  | any ((< 0) . weight) xs = throw $ RandomException "Probability weightings cannot be negative."
  | all ((== 0) . weight) xs = throw $ RandomException "At least one weighting must be positive."
  | otherwise =
      fst . fromJust . (\i -> find ((> i) . snd) (zip xs' $ scanl1 (+) $ weight <$> xs')) <$> randomRIO (0, totalWeight - 1)
  where
    xs' = filter ((> 0) . weight) xs -- removes elements with a weight of zero
    totalWeight = sum $ weight <$> xs'

-- | @chooseOneWeightedWithDefault@ chooses a single random element from a given
-- list with weighted distribution as defined by a given weighting function, or
-- a given default if the list is empty
chooseOneWeightedWithDefault :: a -> (a -> Int) -> [a] -> IO a
chooseOneWeightedWithDefault x weight xs = chooseOneWeighted weight xs `catchBot` \_ -> return x
