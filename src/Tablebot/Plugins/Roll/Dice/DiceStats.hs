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
module Tablebot.Plugins.Roll.Dice.DiceStats (Range (range), getStats) where

import Control.Monad
import Control.Monad.Exception (MonadException)
import Data.Distribution as D hiding (Distribution, fromList)
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
    -- If 0 is always the denominator, the distribution will be empty.
    return $ combineWith div d (assuming (/= 0) d')

instance Range Negation where
  range' (Neg t) = select negate <$> range t
  range' (NoNeg t) = range t

instance Range Expo where
  range' (NoExpo t) = range t
  range' (Expo t e) = do
    d <- range t
    d' <- range e
    -- if the exponent is always negative, the distribution will be empty
    return $ combineWith (^) d (assuming (>= 0) d')

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
    return $
      run $ do
        nbV <- from nbr
        from $ uniform [1 .. nbV]
  range' (CustomDie (LVBList es)) = do
    -- get the distribution for each value in the custom die
    exprs <- mapM range es
    return $ run $ from (uniform exprs) >>= from
  range' cd@(CustomDie _) = evaluationException "tried to find range of complex custom die" [prettyShow cd]

instance Range Dice where
  range' (Dice b d mdor) = do
    b' <- range b
    d' <- range d
    let e = do
          diecount <- from b'
          getDiceExperiment diecount d'
    res <- rangeDiceExperiment d' mdor e
    return $ run $ sum <$> res

-- | Get the distribution of dice values from a given number of dice and the
-- distribution of the die.
getDiceExperiment :: Integer -> Distribution -> Experiment [Integer]
getDiceExperiment i di = replicateM (fromInteger i) (from di)

-- | Go through each operator on dice and modify the `Experiment` representing
-- all possible collections of rolls, returning the `Experiment` produced on
-- finding `Nothing`.
rangeDiceExperiment :: (MonadException m) => Distribution -> Maybe DieOpRecur -> Experiment [Integer] -> m (Experiment [Integer])
rangeDiceExperiment _ Nothing is = return is
rangeDiceExperiment die (Just (DieOpRecur doo mdor)) is = rangeDieOpExperiment die doo is >>= rangeDiceExperiment die mdor

-- | Perform one dice operation on the given `Experiment`, possibly returning
-- a modified experiment representing the distribution of dice rolls.
rangeDieOpExperiment :: MonadException m => Distribution -> DieOpOption -> Experiment [Integer] -> m (Experiment [Integer])
rangeDieOpExperiment die (DieOpOptionLazy o) is = rangeDieOpExperiment die o is
rangeDieOpExperiment _ (DieOpOptionKD kd lhw) is = rangeDieOpExperimentKD kd lhw is
rangeDieOpExperiment die (Reroll rro cond lim) is = do
  limd <- range lim
  return $ do
    limit <- from limd
    let newDie = mkNewDie limit
    rolls <- is
    let (count, cutdownRolls) = countTriggers limit rolls
    if count == 0
      then return cutdownRolls
      else (cutdownRolls ++) <$> getDiceExperiment count newDie
  where
    mkNewDie limitValue
      | rro = die
      | otherwise = assuming (\i -> not $ applyCompare cond i limitValue) die
    countTriggers limitValue = foldr (\i (c, xs') -> if applyCompare cond i limitValue then (c + 1, xs') else (c, i : xs')) (0, [])

-- | Perform a keep/drop operation on the `Experiment` of dice rolls.
rangeDieOpExperimentKD :: (MonadException m) => KeepDrop -> LowHighWhere -> Experiment [Integer] -> m (Experiment [Integer])
rangeDieOpExperimentKD kd (Where cond nb) is = do
  nbDis <- range nb
  return $ do
    wherelimit <- from nbDis
    filter (\i -> keepDrop $ applyCompare cond i wherelimit) <$> is
  where
    keepDrop
      | kd == Keep = id
      | otherwise = not
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
    -- the below exception should never trigger - it is a hold over. it is
    -- present so that this thing type checks nicely.
    whereException = evaluationException "keep/drop where is unsupported" []
    order l l' = if isLow lhw then compare l l' else compare l' l
    sortBy' = sortBy order
    getKeep = if kd == Keep then genericTake else genericDrop
