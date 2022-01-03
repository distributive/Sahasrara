-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStatsBase
-- Description : The basics for dice stats
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The basics for dice stats
module Tablebot.Plugins.Roll.Dice.DiceStatsBase
  ( Distribution,
    toDistribution,
    fromDistribution,
    combineDistributionsBinOp,
    mergeDistributions,
    mergeWeightedDistributions,
    dropWhereDistribution,
    mapOverValue,
    -- getCount,
  )
where

import Data.Map as M
import Diagrams
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Core
import Diagrams.Core.Names
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Graphics.SVGFonts

newtype Distribution = Distribution (Map Integer Rational)
  deriving (Show)

normaliseDistribution :: Distribution -> Distribution
normaliseDistribution (Distribution m) = Distribution $ M.map (/ total) m
  where
    total = M.foldr (+) 0 m

toDistribution :: [(Integer, Rational)] -> Distribution
toDistribution xs = normaliseDistribution $ Distribution $ fromListWith (+) xs

fromDistribution :: Distribution -> [(Integer, Rational)]
fromDistribution (Distribution m) = toList m

combineDistributionsBinOp :: (Integer -> Integer -> Integer) -> Distribution -> Distribution -> Distribution
combineDistributionsBinOp f (Distribution m) (Distribution m') = toDistribution $ combineFunc <$> d <*> d'
  where
    d = toList m
    d' = toList m'
    combineFunc (v, c) (v', c') = (f v v', c * c')

mergeDistributions :: [Distribution] -> Distribution
mergeDistributions ds = normaliseDistribution $ Prelude.foldr helper (Distribution empty) ds
  where
    helper (Distribution d) (Distribution d') = Distribution $ unionWith (+) d d'

mergeWeightedDistributions :: [(Distribution, Rational)] -> Distribution
mergeWeightedDistributions ds = mergeDistributions $ (\(Distribution m, p) -> Distribution $ M.map (* p) m) <$> ds

dropWhereDistribution :: (Integer -> Bool) -> Distribution -> Distribution
dropWhereDistribution f (Distribution m) = normaliseDistribution $ Distribution $ M.filterWithKey (\k _ -> f k) m

mapOverValue :: (Integer -> Integer) -> Distribution -> Distribution
mapOverValue f (Distribution m) = Distribution $ M.mapKeys f m

-- mergeDistributions :: Distribution -> Distribution -> Distribution
-- mergeDistributions (Distribution d) (Distribution d') = normaliseDistribution $ Distribution $ unionWith (+) d d'

toDiagrams :: Distribution -> Diagram B
toDiagrams = undefined
