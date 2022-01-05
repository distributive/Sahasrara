-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStatsBase
-- Description : The basics for dice stats
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The basics for dice stats. Functions for creating and manipulating
-- `Distribution`s.
module Tablebot.Plugins.Roll.Dice.DiceStatsBase
  ( Distribution,
    fromList,
    mergeWeightedDistributions,
    ifInvalidThrow,
    distributionByteString,
  )
where

import Codec.Picture (PngSavable (encodePng))
import Control.Monad.Exception (MonadException)
import qualified Data.ByteString.Lazy as B
import qualified Data.Distribution as D
import Diagrams (Diagram, dims2D, renderDia)
import Diagrams.Backend.Rasterific
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Easy
import Tablebot.Plugins.Roll.Dice.DiceEval (evaluationException)

-- | A wrapper type for mapping values to their probabilities.
type Distribution = D.Distribution Integer

-- | Convenient way to set the types being used so that warnings don't pop up.
fromList :: [(Integer, Rational)] -> Distribution
fromList = D.fromList

-- | If the distribution given is invalid (it is empty), an exception is thrown.
-- Else, the value is just returned.
ifInvalidThrow :: (MonadException m) => Distribution -> m Distribution
ifInvalidThrow d = if D.isValid d then return d else evaluationException "empty distribution" []

-- | Merge all distributions according to a given weighting by multiplying the
-- probabilities in each distribution by the given weighting.
mergeWeightedDistributions :: [(Distribution, Rational)] -> Distribution
mergeWeightedDistributions ds = D.fromList $ do
  (d, r) <- ds
  (i, p) <- D.toList d
  return (i, p * r)

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
    pts = (\(o, s) -> (fromInteger o, [fromRational s])) <$> D.toList d
