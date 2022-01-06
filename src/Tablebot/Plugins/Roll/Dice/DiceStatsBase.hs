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
    distributionByteString,
  )
where

import Codec.Picture (PngSavable (encodePng))
import qualified Data.ByteString.Lazy as B
import qualified Data.Distribution as D
import Diagrams (Diagram, dims2D, renderDia)
import Diagrams.Backend.Rasterific
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Easy

-- | A wrapper type for mapping values to their probabilities.
type Distribution = D.Distribution Integer

-- | Convenient way to set the types being used so that warnings don't pop up.
fromList :: [(Integer, Rational)] -> Distribution
fromList = D.fromList

-- | Merge all distributions according to a given weighting by multiplying the
-- probabilities in each distribution by the given weighting.
mergeWeightedDistributions :: [(Distribution, Rational)] -> Distribution
mergeWeightedDistributions ds = D.fromList $ do
  (d, r) <- ds
  (i, p) <- D.toList d
  return (i, p * r)

-- | Default x and y values for the output chart.
diagramX, diagramY :: Double
(diagramX, diagramY) = (700.0, 400.0)

-- | Get the ByteString representation of the given distribution, setting the
-- string as its title.
distributionByteString :: String -> Distribution -> IO B.ByteString
distributionByteString t d = encodePng . renderDia Rasterific opts <$> distributionDiagram t d
  where
    opts = RasterificOptions (dims2D diagramX diagramY)

-- | Get the Diagram representation of the given distribution, setting the
-- string as its title.
distributionDiagram :: String -> Distribution -> IO (Diagram B)
distributionDiagram t d = do
  defEnv <- defaultEnv (AlignmentFns id id) diagramX diagramY
  return . fst $ runBackendR defEnv r
  where
    r = distributionRenderable t d

-- TODO: make the numbers on the side of the graph have .0 on the end to show they are continuous

-- | Get the Renderable representation of the given distribution, setting the
-- string as its title.
distributionRenderable :: String -> Distribution -> Renderable ()
distributionRenderable t d = toRenderable $ do
  layout_title .= t
  layout_title_style .= defFontStyle
  layout_axes_title_styles .= defFontStyle
  layout_axes_styles .= def {_axis_label_style = defFontStyle}
  layout_x_axis . laxis_title .= "value"
  layout_y_axis . laxis_title .= "probability (%)"
  setColors [opaque blue, opaque red]
  plot $ plotBars <$> bars [""] pts
  where
    pts :: [(Double, [Double])]
    pts = (\(o, s) -> (fromInteger o, [fromRational s * 100])) <$> D.toList d
    defFontStyle = def {_font_size = 2 * _font_size def}
