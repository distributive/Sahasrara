{-|
Module      : Tablebot.Plugins.Ping
Description : A very simple example plugin.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

A collection of utility functions for generating randomness.
-}
module Tablebot.Util.Random (chooseOne, chooseOneWithDefault) where

import System.Random (randomRIO)

-- | @chooseOne@ chooses a single random element from a given list.
chooseOne :: [a] -> IO a
chooseOne [] = fail "Empty list."
chooseOne xs = do
    index <- randomRIO (0, length xs - 1 :: Int)
    return $ head $ drop index xs

-- | @chooseOne@ chooses a single random element from a given list, or a given
-- default value if the list is empty.
chooseOneWithDefault :: a -> [a] -> IO a
chooseOneWithDefault x [] = pure x
chooseOneWithDefault _ xs = do
    index <- randomRIO (0, length xs - 1 :: Int)
    return $ head $ drop index xs
