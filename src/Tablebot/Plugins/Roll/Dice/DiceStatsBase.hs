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
    distributionByteString,
  )
where

-- import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.Core
-- import Diagrams.Core.Names
-- import Diagrams.Prelude hiding (Renderable)
-- import Diagrams.TwoD.GraphViz
-- import Graphics.SVGFonts

import Data.Bifunctor (Bifunctor (second))
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Map as M
import Data.Text qualified as T
import Diagrams (Diagram, dims2D, renderDia, mkWidth)
-- import Diagrams.Backend.SVG
import Diagrams.Backend.Rasterific
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Easy
import Codec.Picture (PngSavable(encodePng))

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
mergeDistributions ds = normaliseDistribution $ Prelude.foldr helper (Distribution M.empty) ds
  where
    helper (Distribution d) (Distribution d') = Distribution $ unionWith (+) d d'

mergeWeightedDistributions :: [(Distribution, Rational)] -> Distribution
mergeWeightedDistributions ds = mergeDistributions $ (\(Distribution m, p) -> Distribution $ M.map (* p) m) <$> ds

dropWhereDistribution :: (Integer -> Bool) -> Distribution -> Distribution
dropWhereDistribution f (Distribution m) = normaliseDistribution $ Distribution $ M.filterWithKey (\k _ -> f k) m

mapOverValue :: (Integer -> Integer) -> Distribution -> Distribution
mapOverValue f (Distribution m) = Distribution $ M.mapKeysWith (+) f m

distributionByteString :: String -> Distribution -> IO B.ByteString
distributionByteString t d = encodePng . renderDia Rasterific opts <$> distributionDiagram t d
  where
    opts = RasterificOptions (dims2D 700 400)

distributionDiagram :: String -> Distribution -> IO (Diagram B)
distributionDiagram t d = do
  defEnv <- defaultEnv (AlignmentFns id id) 700 400
  return . fst $ runBackendR defEnv r
  where
    r = distributionRenderable t d

distributionRenderable :: String -> Distribution -> Renderable ()
distributionRenderable t d = toRenderable $ do
  layout_title .= t
  setColors [opaque blue, opaque red]
  plot $ plotBars <$> bars ["values"] pts
  where
    pts :: [(Double, [Double])]
    pts = (\(o, s) -> (fromInteger o, [ fromRational s])) <$> fromDistribution d

