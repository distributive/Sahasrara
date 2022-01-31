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
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import qualified Data.Distribution as D
import Data.List (genericLength)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Diagrams (Diagram, dims2D, renderDia)
import Diagrams.Backend.Rasterific
import Graphics.Rendering.Chart.Axis.Int
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

-- | Get the Renderable representation of the given distribution, setting the
-- string as its title.
distributionRenderable :: [(Distribution, T.Text)] -> Renderable ()
distributionRenderable d = toRenderable $ do
  layout_title .= T.unpack (title' d)
  layout_x_axis . laxis_title .= "value"
  layout_y_axis . laxis_title .= "probability (%)"
  layout_x_axis . laxis_generate .= scaledIntAxis' r
  layout_y_axis . laxis_override .= \ad@AxisData {_axis_labels = axisLabels} -> ad {_axis_labels = (second (\s -> if '.' `elem` s then s else s ++ ".0") <$>) <$> axisLabels}
  layout_all_font_styles .= defFontStyle
  pb <- (bars @Integer @Double) (barNames d) pts
  let pb' = pb {_plot_bars_spacing = BarsFixGap 10 5}
  plot $ return $ plotBars pb'
  where
    removeNullMap m
      | M.null m = M.singleton 0 0
      | otherwise = m
    ds = removeNullMap . D.toMap . fst <$> d
    allIntegers = let s = S.unions $ M.keysSet <$> ds in [S.findMin s .. S.findMax s]
    insertEmpty k = M.insertWith (\_ a -> a) k 0
    ds' = M.unionsWith (++) $ M.map (: []) <$> (applyAll (insertEmpty <$> allIntegers) <$> ds)
    pts = second (fromRational . (* 100) <$>) <$> M.toList ds'
    r = (fst $ M.findMin ds', fst $ M.findMax ds')
    applyAll [] = id
    applyAll (f : fs) = f . applyAll fs
    defFontStyle = def {_font_size = 2 * _font_size def}
    barNames [_] = [""]
    barNames xs = T.unpack . snd <$> xs
    title' [(_, t)] = t
    title' xs = "Range of " <> T.intercalate ", " (snd <$> xs)

-- | Custom scaling function due to some difficulties for drawing charts.
--
-- Using
-- https://hackage.haskell.org/package/Chart-1.9.3/docs/src/Graphics.Rendering.Chart.Axis.Int.html#scaledIntAxis
-- for pointers.
scaledIntAxis' :: (Integer, Integer) -> AxisFn Integer
scaledIntAxis' r@(minI, maxI) _ = makeAxis (_la_labelf lap) ((minI - 1) : (maxI + 1) : labelvs, tickvs, gridvs)
  where
    lap = defaultIntAxis
    labelvs = stepsInt' (fromIntegral $ _la_nLabels lap) r
    tickvs =
      stepsInt'
        (fromIntegral $ _la_nTicks lap)
        ( fromIntegral $ minimum labelvs,
          fromIntegral $ maximum labelvs
        )
    gridvs = labelvs

-- | Taken and modified from
-- https://hackage.haskell.org/package/Chart-1.9.3/docs/src/Graphics.Rendering.Chart.Axis.Int.html#stepsInt
stepsInt' :: Integer -> (Integer, Integer) -> [Integer]
stepsInt' nSteps range = bestSize (goodness alt0) alt0 alts
  where
    bestSize n a (a' : as) =
      let n' = goodness a'
       in if n' < n then bestSize n' a' as else a
    bestSize _ _ [] = []

    goodness vs = abs (genericLength vs - nSteps)

    (alt0 : alts) = map (`steps` range) sampleSteps'

    -- throw away sampleSteps that are definitely too small as
    -- they takes a long time to process
    sampleSteps' =
      let rangeMag = (snd range - fst range)

          (s1, s2) = span (< (rangeMag `div` nSteps)) sampleSteps
       in (reverse . take 5 . reverse) s1 ++ s2

    -- generate all possible step sizes
    sampleSteps = [1, 2, 5] ++ sampleSteps1
    sampleSteps1 = [10, 20, 25, 50] ++ map (* 10) sampleSteps1

    steps :: Integer -> (Integer, Integer) -> [Integer]
    steps size' (minV, maxV) = takeWhile (< b) [a, a + size' ..] ++ [b]
      where
        a = floor @Double (fromIntegral minV / fromIntegral size') * size'
        b = ceiling @Double (fromIntegral maxV / fromIntegral size') * size'
