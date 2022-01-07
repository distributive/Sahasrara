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
    distributionByteString,
  )
where

import Codec.Picture (PngSavable (encodePng))
import Data.ByteString.Lazy qualified as B
import Data.Distribution qualified as D
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Diagrams (Diagram, dims2D, renderDia)
import Diagrams.Backend.Rasterific
import Graphics.Rendering.Chart.Backend.Diagrams (defaultEnv, runBackendR)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Easy
import Tablebot.Plugins.Roll.Dice.DiceEval (evaluationException)

-- | A wrapper type for mapping values to their probabilities.
type Distribution = D.Distribution Integer

-- | Default x and y values for the output chart.
diagramX, diagramY :: Double
(diagramX, diagramY) = (1400.0, 400.0)

-- | Get the ByteString representation of the given distribution, setting the
-- string as its title.
distributionByteString :: [(Distribution, T.Text)] -> IO B.ByteString
distributionByteString d = encodePng . renderDia Rasterific opts <$> distributionDiagram d
  where
    opts = RasterificOptions (dims2D diagramX diagramY)

-- | Get the Diagram representation of the given distribution, setting the
-- string as its title.
distributionDiagram :: [(Distribution, T.Text)] -> IO (Diagram B)
distributionDiagram d = do
  if null d
    then evaluationException "empty distribution" []
    else do
      defEnv <- defaultEnv (AlignmentFns id id) diagramX diagramY
      return . fst $ runBackendR defEnv r
  where
    r = distributionRenderable d

-- TODO: make the numbers on the side of the graph have .0 on the end to show they are continuous

-- | Get the Renderable representation of the given distribution, setting the
-- string as its title.
distributionRenderable :: [(Distribution, T.Text)] -> Renderable ()
distributionRenderable d = toRenderable $ do
  layout_title .= T.unpack (title' d)
  layout_title_style .= defFontStyle
  layout_axes_title_styles .= defFontStyle
  layout_axes_styles .= def {_axis_label_style = defFontStyle}
  layout_x_axis . laxis_title .= "value"
  layout_y_axis . laxis_title .= "probability (%)"
  -- setColors (take[opaque blue, opaque red, opaque green, opaque teal, opaque violet])
  plot $ plotBars <$> (bars @Double @Double) (barNames d)  pts
  where
    ds = M.fromList . D.toList . fst <$> d
    allIntegers = S.toList $ S.unions $ M.keysSet <$> ds
    insertEmpty k = M.insertWith (\_ a -> a) k 0
    ds' = M.unionsWith (++) $ M.map (: []) <$> (applyAll (insertEmpty <$> allIntegers) <$> ds)
    pts = bimap fromInteger (fromRational . (*100) <$>) <$> M.toList ds'
    applyAll [] = id
    applyAll (f:fs) = f . applyAll fs
    defFontStyle = def {_font_size = 2 * _font_size def}
    barNames [_] = [""]
    barNames xs = T.unpack . snd <$> xs
    title' [(_, t)] = t
    title' xs = "Range of " <> T.intercalate ", " (snd <$> xs)
