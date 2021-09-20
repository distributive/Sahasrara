{-|
Module      : Tablebot.Util.Random
Description : A very simple example plugin.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

A collection of utility functions for generating randomness.
-}
module Tablebot.Util.Random (chooseOne, chooseOneWithDefault, chooseOneWeighted) where

import System.Random (randomRIO)

-- | @chooseOne@ chooses a single random element from a given list with uniform
-- distribution.
chooseOne :: [a] -> IO a
chooseOne [] = fail "Empty list."
chooseOne xs = do
    index <- randomRIO (0, length xs - 1 :: Int)
    return $ xs !! index

-- | @chooseOneWithDefault@ chooses a single random element from a given list
-- with uniform distribution, or a given default value if the list is empty.
chooseOneWithDefault :: a -> [a] -> IO a
chooseOneWithDefault x [] = pure x
chooseOneWithDefault _ xs = chooseOne xs

-- | @chooseOneWeighted@ chooses a single random element from a given list with
-- weighted distribution as defined by a given weighting function.
chooseOneWeighted :: (a -> Int) -> [a] -> IO a
chooseOneWeighted weight xs = do
    index <- randomRIO (0, (sum $ weight <$> xs) - 1)
    return $ fst $ foldr iter (head xs, index) xs
        where
            iter new (old, i)
                | i <= 0    = (old, i)
                | otherwise = (new, i - weight new)

-- | @chooseOneWeightedWithDefault@ chooses a single random element from a given
-- list with weighted distribution as defined by a given weighting function, or
-- a given default if the list is empty
chooseOneWeightedWithDefault :: a -> (a -> Int) -> [a] -> IO a
chooseOneWeightedWithDefault x _ [] = pure x
chooseOneWeightedWithDefault _ weight xs = chooseOneWeighted weight xs
