-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStats
-- Description : Get statistics on particular expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin generates statistics based on the values of dice in given
-- expressions.
module Tablebot.Plugins.Roll.Dice.DiceStats where

import Control.Monad
import Control.Monad.Exception (MonadException)
import Data.Functor ((<&>))
import Data.List
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceStatsBase

-- | Get the most common values, the mean, and the standard deviation of a given
-- distribution.
getStats :: Distribution -> ([Integer], Double, Double)
getStats d = (modalOrder, fromRational mean, std)
  where
    vals = fromDistribution d
    (mean, _, _) = Prelude.foldr (\(i, r) (a, c, nz) -> (fromInteger i * r + a, c + 1, nz + fromIntegral (fromEnum (r /= 0)))) (0, 0 :: Integer, 0 :: Integer) vals
    modalOrder = fst <$> sortBy (\(_, r) (_, r') -> compare r' r) vals
    -- https://stats.stackexchange.com/a/295015
    std = sqrt $ fromRational $ sum ((\(x, w) -> w * (fromInteger x - mean) * (fromInteger x - mean)) <$> vals)

-- | Convenience wrapper for
-- `Tablebot.Plugins.Roll.Dice.DiceStatsBase.combineDistributionsBinOp`, which
-- gets the range of the given values then applies the function to the resultant
-- distributions.
combineRangesBinOp :: (MonadException m, Range a, Range b) => (Integer -> Integer -> Integer) -> a -> b -> m Distribution
combineRangesBinOp f a b = do
  d <- range a
  d' <- range b
  combineDistributionsBinOp f d d'

-- | Type class to get the overall range of a value.
--
-- A `Tablebot.Plugins.Roll.Dice.DiceStatsBase.Distribution` is a map of values
-- to probabilities, and has a variety of functions that operate on them.
class Range a where
  -- | Try and get the `Distribution` of the given value, throwing a
  -- `MonadException` on failure.
  range :: MonadException m => a -> m Distribution

instance Range Expr where
  range (NoExpr t) = range t
  range (Add t e) = combineRangesBinOp (+) t e
  range (Sub t e) = combineRangesBinOp (-) t e

instance Range Term where
  range (NoTerm t) = range t
  range (Multi t e) = combineRangesBinOp (*) t e
  range (Div t e) = do
    d <- range t
    d' <- range e
    -- having 0 as a denominator is bad
    d'' <- dropWhereDistribution (== 0) d'
    combineDistributionsBinOp div d d''

instance Range Negation where
  range (Neg t) = do
    d <- range t
    return $ mapOverValue negate d
  range (NoNeg t) = range t

instance Range Expo where
  range (NoExpo t) = range t
  range (Expo t e) = do
    d <- range t
    d' <- range e
    d'' <- dropWhereDistribution (>= 0) d'
    combineDistributionsBinOp (^) d d''

instance Range Func where
  range (NoFunc t) = range t
  range f@(Func _ _) = evaluationException "tried to find range of function" [prettyShow f]

instance Range NumBase where
  range (Value i) = toDistribution [(i, 1)]
  range (NBParen (Paren e)) = range e

instance Range Base where
  range (NBase nb) = range nb
  range (DiceBase d) = range d

instance Range Die where
  range (LazyDie d) = range d
  range (Die nb) = do
    nbr <- range nb
    vcs <- sequence $ (\(hv, p) -> toDistribution ((,1 / fromIntegral hv) <$> [1 .. hv]) <&> (,p)) <$> fromDistribution nbr
    mergeWeightedDistributions vcs
  range (CustomDie (LVBList es)) = do
    exprs <- mapM range es
    let l = fromIntegral $ length es
    mergeWeightedDistributions ((,1 / l) <$> exprs)
  range cd@(CustomDie _) = evaluationException "tried to find range of complex custom die" [prettyShow cd]

instance Range Dice where
  range (Dice b d mdor) = do
    b' <- range b
    d' <- range d
    dcs <- rangeDieOp d' mdor [(b', d', 1)] >>= sequence . (fromCountAndDie <$>)
    mergeWeightedDistributions dcs

type DiceCollection = (Distribution, Distribution, Rational)

fromCountAndDie :: MonadException m => DiceCollection -> m (Distribution, Rational)
fromCountAndDie (c, d, r) = do
  mwd <- sequence $ do
    (i, p) <- fromDistribution c
    if i < 1
      then []
      else do
        let v = Prelude.foldr1 (\a b -> a >>= \a' -> b >>= \b' -> combineDistributionsBinOp (+) a' b') (genericTake i (repeat (return d)))
        [v <&> (,p)]
  mwd' <- mergeWeightedDistributions mwd
  return (mwd', r)

rangeDieOp :: (MonadException m) => Distribution -> Maybe DieOpRecur -> [DiceCollection] -> m [DiceCollection]
rangeDieOp _ Nothing ds = return ds
rangeDieOp die (Just (DieOpRecur doo mdor)) ds = rangeDieOp' die doo ds >>= rangeDieOp die mdor

rangeDieOp' :: forall m. MonadException m => Distribution -> DieOpOption -> [DiceCollection] -> m [DiceCollection]
rangeDieOp' die (DieOpOptionLazy o) ds = rangeDieOp' die o ds
rangeDieOp' _ (DieOpOptionKD kd lhw) ds = rangeDieOpHelpKD kd lhw ds
rangeDieOp' die (Reroll rro cond lim) ds = do
  limd <- range lim
  join
    <$> sequence
      ( do
          (v, p) <- fromDistribution limd
          return
            ( do
                nd <- die' v
                sequence
                  ( do
                      (c, d, cp) <- ds
                      let d' = do
                            (dieV, dieP) <- fromDistribution d
                            if applyCompare cond dieV v
                              then [return (nd, dieP)]
                              else [toDistribution [(dieV, 1)] <&> (,dieP)]

                      return $
                        sequence
                          d'
                          >>= ( mergeWeightedDistributions
                                  >=> (\mwd -> return (c, mwd, cp * p))
                              )
                  )
            )
      )
  where
    die' :: forall m. (MonadException m) => Integer -> m Distribution
    die' v
      | rro = return die
      | otherwise = do
        d <- dropWhereDistribution (\i -> not $ applyCompare cond i v) die
        if nullDistribution d
          then evaluationException "cannot reroll die infinitely; range is incorrect" []
          else return d

rangeDieOpHelpKD :: (MonadException m) => KeepDrop -> LowHighWhere -> [DiceCollection] -> m [DiceCollection]
rangeDieOpHelpKD kd lhw ds = do
  let nb = getValueLowHigh lhw
  case nb of
    Nothing -> whereException
    Just nb' -> do
      repeatType <- chooseType kd lhw
      nbd <- range nb'
      sequence
        ( do
            (i, p) <- fromDistribution nbd
            (c, d, dcp) <- ds
            (ci, cp) <- fromDistribution c
            let toKeep = getRemaining ci i
                d' = repeatType (ci - toKeep) d
            return $ d' >>= \d'' -> toDistribution [(toKeep, 1)] <&> (,d'',p * dcp * cp)
        )
  where
    whereException = evaluationException "keep/drop where is unsupported" []
    getRemaining total value
      | kd == Keep = min total value
      | otherwise = max 0 (total - value)
    repeatedM m i d
      | i <= 0 = return d
      | otherwise = repeatedM m (i - 1) d >>= combineDistributionsBinOp m d
    repeatedMinimum = repeatedM min
    repeatedMaximum = repeatedM max
    chooseType Keep (High _) = return repeatedMaximum
    chooseType Keep (Low _) = return repeatedMinimum
    chooseType Drop (Low _) = return repeatedMaximum
    chooseType Drop (High _) = return repeatedMaximum
    chooseType _ _ = evaluationException "keep/drop where is unsupported" []
