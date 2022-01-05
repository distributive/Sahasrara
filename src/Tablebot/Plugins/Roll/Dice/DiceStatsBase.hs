-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStatsBase
-- Description : The basics for dice stats
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The basics for dice stats. Functions for creating and manipulating
-- `Distribution`s. The constructor for `Distribution` is not exported to ensure
-- that a given `Distribution` is valid.
module Tablebot.Plugins.Roll.Dice.DiceStatsBase
  ( Distribution,
    toDistribution,
    fromDistribution,
    combineDistributionsBinOp,
    mergeWeightedDistributions,
    dropWhereDistribution,
    mapOverValue,
    distributionByteString,
    nullDistribution,
  )
where

import Codec.Picture (PngSavable (encodePng))
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Diagrams (Diagram, dims2D, renderDia)
import Diagrams.Backend.Rasterific
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Easy

-- | A wrapper type for mapping values to their probabilities.
--
-- The constructor is not exported to ensure that the Distribution is always
-- valid.
newtype Distribution = Distribution (M.Map Integer Rational)
  deriving (Show)

-- | Check whether the distribution is empty.
nullDistribution :: Distribution -> Bool
nullDistribution (Distribution m) = M.null m

-- | Given a distribution, normalise the probabilities so that they sum to 1.
normaliseDistribution :: Distribution -> Distribution
normaliseDistribution d@(Distribution m) = if M.null m then d else Distribution $ M.map (/ total) m
  where
    total = M.foldr (+) 0 m

-- | Turn a list of integer-rational tuples into a Distribution. Normalises so
-- that the Distribution is valid.
toDistribution :: [(Integer, Rational)] -> Distribution
toDistribution [(i, _)] = Distribution (M.singleton i 1)
toDistribution xs = normaliseDistribution $ Distribution $ M.fromListWith (+) xs

-- | Get the integer-rational tuples that represent a distribution.
fromDistribution :: Distribution -> [(Integer, Rational)]
fromDistribution (Distribution m) = M.toList m

-- | Combine two distributions by applying the given function between every
-- element of each one, returning the resultant distribution.
combineDistributionsBinOp :: (Integer -> Integer -> Integer) -> Distribution -> Distribution -> Distribution
combineDistributionsBinOp f (Distribution m) (Distribution m') = toDistribution $ combineFunc <$> d <*> d'
  where
    d = M.toList m
    d' = M.toList m'
    combineFunc (v, c) (v', c') = (f v v', c * c')

-- | Merge all distributions by adding together the probabilities of any values
-- that are in multiple distributions, and normalising at the end.
mergeDistributions :: [Distribution] -> Distribution
mergeDistributions ds = normaliseDistribution $ Prelude.foldr helper (Distribution M.empty) ds
  where
    helper (Distribution d) (Distribution d') = Distribution $ M.unionWith (+) d d'

-- | Merge all distributions according to a given weighting by multiplying the
-- probabilities in each distribution by the given weighting. Uses
-- `mergeDistributions`.
mergeWeightedDistributions :: [(Distribution, Rational)] -> Distribution
mergeWeightedDistributions ds = mergeDistributions $ (\(Distribution m, p) -> Distribution $ M.map (* p) m) <$> ds

-- | Drop all items in the distribution that fulfill the given function.
dropWhereDistribution :: (Integer -> Bool) -> Distribution -> Distribution
dropWhereDistribution f (Distribution m) = normaliseDistribution $ Distribution $ M.filterWithKey (\k _ -> f k) m

-- | Map over all the integer values, combining the probabilities that then map
-- to the same integer.
mapOverValue :: (Integer -> Integer) -> Distribution -> Distribution
mapOverValue f (Distribution m) = Distribution $ M.mapKeysWith (+) f m

-- | Get the ByteString representation of the given distribution, setting the
-- string as its title.
distributionByteString :: String -> Distribution -> IO B.ByteString
distributionByteString t d = encodePng . renderDia Rasterific opts <$> distributionDiagram t d
  where
    opts = RasterificOptions (dims2D 700 400)

-- | Get the Diagram representation of the given distribution, setting the
-- string as its title.
distributionDiagram :: String -> Distribution -> IO (Diagram B)
distributionDiagram t d = do
  defEnv <- defaultEnv (AlignmentFns id id) 700 400
  return . fst $ runBackendR defEnv r
  where
    r = distributionRenderable t d

-- | Get the Renderable representation of the given distribution, setting the
-- string as its title.
distributionRenderable :: String -> Distribution -> Renderable ()
distributionRenderable t d = toRenderable $ do
  layout_title .= t
  setColors [opaque blue, opaque red]
  plot $ plotBars <$> bars ["values"] pts
  where
    pts :: [(Double, [Double])]
    pts = (\(o, s) -> (fromInteger o, [fromRational s])) <$> fromDistribution d
