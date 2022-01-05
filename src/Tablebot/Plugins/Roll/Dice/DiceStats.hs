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
import Data.Distribution as D hiding (Distribution, fromList)
import Data.Functor ((<&>))
import Data.List
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceStatsBase

-- | Get the most common values, the mean, and the standard deviation of a given
-- distribution.
getStats :: Distribution -> ([Integer], Double, Double)
getStats d = (modalOrder, expectation d, standardDeviation d)
  where
    vals = toList d
    modalOrder = fst <$> sortBy (\(_, r) (_, r') -> compare r' r) vals

-- | Convenience wrapper which gets the range of the given values then applies
-- the function to the resultant distributions.
combineRangesBinOp :: (MonadException m, Range a, Range b, PrettyShow a, PrettyShow b) => (Distribution -> Distribution -> Distribution) -> a -> b -> m Distribution
combineRangesBinOp f a b = do
  d <- range a
  d' <- range b
  return $ f d d'

-- | Type class to get the overall range of a value.
--
-- A `Tablebot.Plugins.Roll.Dice.DiceStatsBase.Distribution` is a map of values
-- to probabilities, and has a variety of functions that operate on them.
class Range a where
  -- | Try and get the `Distribution` of the given value, throwing a
  -- `MonadException` on failure.
  range :: (MonadException m, PrettyShow a) => a -> m Distribution
  range a = propagateException (prettyShow a) (range' a)

  range' :: (MonadException m, PrettyShow a) => a -> m Distribution

instance Range Expr where
  range' (NoExpr t) = range t
  range' (Add t e) = combineRangesBinOp (+) t e
  range' (Sub t e) = combineRangesBinOp (-) t e

instance Range Term where
  range' (NoTerm t) = range t
  range' (Multi t e) = combineRangesBinOp (*) t e
  range' (Div t e) = do
    d <- range t
    d' <- range e
    -- having 0 as a denominator is disallowed
    d'' <- ifInvalidThrow $ assuming (/= 0) d'
    return $ combineWith div d d''

instance Range Negation where
  range' (Neg t) = select negate <$> range t
  range' (NoNeg t) = range t

instance Range Expo where
  range' (NoExpo t) = range t
  range' (Expo t e) = do
    d <- range t
    d' <- range e
    -- having negative values is disallowed
    d'' <- ifInvalidThrow $ assuming (>= 0) d'
    return $ combineWith (^) d d''

instance Range Func where
  range' (NoFunc t) = range t
  range' f@(Func _ _) = evaluationException "tried to find range of function, which is currently unsupported" [prettyShow f]

instance Range NumBase where
  range' (Value i) = return $ always i
  range' (NBParen (Paren e)) = range e

instance Range Base where
  range' (NBase nb) = range nb
  range' (DiceBase d) = range d

instance Range Die where
  range' (LazyDie d) = range d
  range' (Die nb) = do
    nbr <- range nb
    -- for each possible nb value, create a (Distribution, Rational) pair
    -- representing the distribution of the die and the probability of that
    -- distribution coming up
    let vcs = (\(hv, p) -> (fromList ((,1 / fromIntegral hv) <$> [1 .. hv]), p)) <$> toList nbr
    -- then condense that into a single distribution
    ifInvalidThrow $ mergeWeightedDistributions vcs
  range' (CustomDie (LVBList es)) = do
    -- get the distribution for each value in the custom die
    exprs <- mapM range es
    let l = genericLength es
    -- then merge all the distributions. each distribution is equally likely to
    -- come up.
    ifInvalidThrow $ mergeWeightedDistributions ((,1 / l) <$> exprs)
  range' cd@(CustomDie _) = evaluationException "tried to find range of complex custom die" [prettyShow cd]

instance Range Dice where
  range' (Dice b d mdor) = do
    b' <- range b
    d' <- range d
    dcs <- rangeDieOp d' mdor [(b', d', 1)] >>= sequence . (fromCountAndDie <$>)
    return $ mergeWeightedDistributions dcs

-- | Aliased type to represent a singular instance of (number of dice,
-- distribution of a die, the probability of this occuring).
type DiceCollection = (Distribution, Distribution, Rational)

-- | From a `DiceCollection`, get a distribution and the probability of that
-- distribution.
fromCountAndDie :: MonadException m => DiceCollection -> m (Distribution, Rational)
fromCountAndDie (c, d, r) = do
  let mwd = do
        (i, p) <- toList c
        return $
          if i < 1
            then (fromList [(0, 1)], p)
            else do
              let v = sum (genericTake i (repeat d))
              (v, p)
  return (mergeWeightedDistributions mwd, r)

-- | Step by step apply `rangeDieOp'`, returning the current list of
-- `DiceCollection`s when `Nothing` is encountered.
rangeDieOp :: (MonadException m) => Distribution -> Maybe DieOpRecur -> [DiceCollection] -> m [DiceCollection]
rangeDieOp _ Nothing ds = return ds
rangeDieOp die (Just (DieOpRecur doo mdor)) ds = rangeDieOp' die doo ds >>= rangeDieOp die mdor

rangeDiceExperiment :: (MonadException m) => Distribution -> Maybe DieOpRecur -> Experiment [Integer] -> m (Experiment [Integer])
rangeDiceExperiment _ Nothing is = return is
rangeDiceExperiment die (Just (DieOpRecur doo mdor)) is = rangeDieOpExperiment die doo is >>= rangeDiceExperiment die mdor

rangeDieOpExperiment :: MonadException m => Distribution -> DieOpOption -> Experiment [Integer] -> m (Experiment [Integer])
rangeDieOpExperiment die (DieOpOptionLazy o) is = rangeDieOpExperiment die o is
rangeDieOpExperiment die (DieOpOptionKD kd lhw) is = rangeDieOpExperimentKD kd lhw is

rangeDieOpExperimentKD :: (MonadException m) => KeepDrop -> LowHighWhere -> Experiment [Integer] -> m (Experiment [Integer])
rangeDieOpExperimentKD kd lhw is = do
  let nb = getValueLowHigh lhw
  case nb of
    Nothing -> whereException
    Just nb' -> do
      nbd <- range nb'
      return $ do
        kdlh <- from nbd
        getKeep kdlh . sortBy' <$> is
  where
    whereException = evaluationException "keep/drop where is unsupported" []
    order l l' = if isLow lhw then compare l l' else compare l' l
    sortBy' = sortBy order
    getKeep = if kd == Keep then genericTake else genericDrop

-- | Apply a single `DieOpOption` to the current list of `DiceCollection`s.
rangeDieOp' :: forall m. MonadException m => Distribution -> DieOpOption -> [DiceCollection] -> m [DiceCollection]
rangeDieOp' die (DieOpOptionLazy o) ds = rangeDieOp' die o ds
rangeDieOp' _ (DieOpOptionKD kd lhw) ds = rangeDieOpHelpKD kd lhw ds
rangeDieOp' die (Reroll rro cond lim) ds = do
  limd <- range lim
  -- join together the nested lists, as well as sequencing the
  -- `MonadException` values
  join
    <$> sequence
      ( do
          -- for each possible value of the limit, perform the rest of the input
          (limitValue, limitProbability) <- toList limd
          return
            ( -- get the new die distribution (only relevant on infinite
              -- rerolls). if the die distribution is invalid (no values), an
              -- exception is thrown here, as early as possible.
              -- then, transform the given dice collections
              die' limitValue <&> transformDiceCollections limitValue limitProbability
            )
      )
  where
    die' limitValue
      | rro = return die
      | otherwise = let d = assuming (\i -> not $ applyCompare cond i limitValue) die in ifInvalidThrow d

    -- Go through all the dice values and conditionally perform the reroll.
    conditionallyReroll dieDistribution limitValue newDie = do
      (dieV, dieP) <- toList dieDistribution
      if applyCompare cond dieV limitValue
        then [(newDie, dieP)]
        else [(fromList [(dieV, 1)], dieP)]
    transformDiceCollections limitValue limitProbability newDie =
      do
        -- for each dice collection in the list, perform  the
        -- below.
        (c, dieDistribution, cp) <- ds
        -- return the list of dice collections, sequencing as
        -- needed
        let mwd = mergeWeightedDistributions $ conditionallyReroll dieDistribution limitValue newDie
        return (c, mwd, cp * limitProbability)

-- | Apply a keep/drop dice operation using the given `LowHighWhere` onto the
-- list of `DiceCollection`s.
rangeDieOpHelpKD :: (MonadException m) => KeepDrop -> LowHighWhere -> [DiceCollection] -> m [DiceCollection]
rangeDieOpHelpKD kd lhw ds = do
  let nb = getValueLowHigh lhw
  case nb of
    Nothing -> whereException
    Just nb' -> do
      repeatType <- chooseType kd lhw
      nbd <- range nb'
      return
        ( do
            (i, p) <- toList nbd
            (c, d, dcp) <- ds
            (ci, cp) <- toList c
            let toKeep = getRemaining ci i
                d' = repeatType (ci - toKeep) d
            return (fromList [(toKeep, 1 :: Rational)], d', p * dcp * cp)
        )
  where
    whereException = evaluationException "keep/drop where is unsupported" []
    getRemaining total value
      | kd == Keep = min total value
      | otherwise = max 0 (total - value)
    repeatedM m i d
      | i <= 0 = d
      | otherwise = combineWith m d $ repeatedM m (i - 1) d
    repeatedMinimum = repeatedM min
    repeatedMaximum = repeatedM max
    chooseType Keep (High _) = return repeatedMaximum
    chooseType Keep (Low _) = return repeatedMinimum
    chooseType Drop (Low _) = return repeatedMaximum
    chooseType Drop (High _) = return repeatedMinimum
    chooseType _ _ = evaluationException "keep/drop where is unsupported" []
