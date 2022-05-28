-- |
-- Module      : Sahasrara.Utility.Random
-- Description : A plugin for random generation.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- A collection of utility functions for generating randomness.
module Sahasrara.Utility.Random
  ( randomRange,
    randomRangeSeeded,
    shuffle,
    chooseOne,
    chooseOneWithDefault,
    chooseOneSeeded,
    chooseOneWeighted,
    chooseOneWeightedWithDefault,
    chooseN,
  )
where

import Control.Monad (foldM)
import Control.Monad.Exception (MonadException (throw))
import Data.List (find)
import Data.Maybe (fromJust)
import Sahasrara.Utility.Exception (BotException (RandomException), catchBot)
import System.Random (mkStdGen, randomR, randomRIO)

-- | @randomRange@ gets a random number between lower (inclusive) and upper
-- (exclusive).
-- Behaviour is undefined for lower >= upper.
randomRange :: Int -> Int -> IO Int
randomRange lower upper = randomRIO (lower, upper - 1)

-- | @randomRangeSeeded@ gets a random number between lower (inclusive) and
-- upper (exclusive), based on a single seed.
-- Behaviour is undefined for lower >= upper.
randomRangeSeeded :: Int -> Int -> Int -> Int
randomRangeSeeded seed lower upper = fst $ randomR (lower, upper - 1 :: Int) $ mkStdGen seed

-- | @shuffle@ randomly reorders a list.
shuffle :: [a] -> IO [a]
shuffle ls = foldM swap ls [0 .. length ls - 1]
  where
    swap :: [a] -> Int -> IO [a]
    swap xs a = do
      b <- randomRange 0 $ length xs
      let lo = if a > b then b else a
          hi = if a < b then b else a
          loVal = xs !! lo
          hiVal = xs !! hi
          bottom = take lo xs
          middle = take (hi - lo - 1) $ drop (lo + 1) xs
          top = drop (hi + 1) xs
      return $
        if a == b
          then xs
          else bottom ++ [hiVal] ++ middle ++ [loVal] ++ top

-- | @chooseOne@ chooses a single random element from a given list with uniform
-- distribution.
chooseOne :: [a] -> IO a
chooseOne [] = throw $ RandomException "Cannot choose from empty list."
chooseOne xs = (xs !!) <$> randomRange 0 (length xs)

-- | @chooseOneWithDefault@ chooses a single random element from a given list
-- with uniform distribution, or a given default value if the list is empty.
chooseOneWithDefault :: a -> [a] -> IO a
chooseOneWithDefault x xs = chooseOne xs `catchBot` \_ -> return x

-- | @chooseOneSeeded@ chooses a single random element from a given list
-- with uniform distribution, based on a single seed.
chooseOneSeeded :: Int -> [a] -> IO a
chooseOneSeeded _ [] = throw $ RandomException "Cannot choose from empty list."
chooseOneSeeded seed xs = return $ (!!) xs $ randomRangeSeeded seed 0 (length xs)

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
    fst . fromJust . (\i -> find ((> i) . snd) (zip xs' $ scanl1 (+) $ weight <$> xs')) <$> randomRange 0 totalWeight
  where
    xs' = filter ((> 0) . weight) xs -- removes elements with a weight of zero
    totalWeight = sum $ weight <$> xs'

chooseN :: Int -> [a] -> IO [a]
chooseN _ [] = throw $ RandomException "Cannot choose from empty list."
chooseN n xs = take n <$> shuffle xs

-- | @chooseOneWeightedWithDefault@ chooses a single random element from a given
-- list with weighted distribution as defined by a given weighting function, or
-- a given default if the list is empty
chooseOneWeightedWithDefault :: a -> (a -> Int) -> [a] -> IO a
chooseOneWeightedWithDefault x weight xs = chooseOneWeighted weight xs `catchBot` \_ -> return x
