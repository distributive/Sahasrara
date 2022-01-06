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
import Control.Monad.Exception
import Data.Bifunctor (Bifunctor (first))
import Data.Distribution hiding (Distribution, fromList)
import qualified Data.Distribution as D
import Data.List
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceFunctions
import Tablebot.Plugins.Roll.Dice.DiceStatsBase (Distribution)
import Tablebot.Utility.Exception (catchBot)

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
-- A `Data.Distribution.Distribution` is a map of values to probabilities, and
-- has a variety of  functions that operate on them.
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
  range' (Func fi avs) = rangeFunction fi avs

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
  range' (CustomDie lv) = do
    dievs <- rangeList lv
    return $ run $ from dievs >>= from . uniform

instance Range Dice where
  range' (Dice b d mdor) = do
    b' <- range b
    d' <- range d
    let e = do
          diecount <- from b'
          getDiceExperiment diecount d'
    res <- rangeDiceExperiment d' mdor e
    return $ run $ sum <$> res

-- | Get the distribution of values from a given number of (identically
-- distributed) values and the distribution of that value.
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

-- | Convenient alias for a distribution of lists of integers.
type DistributionList = D.Distribution [Integer]

-- | Type class to get the overall range of a list of values.
--
-- Only used within `DiceStats` as I have no interest in producing statistics on
-- lists
class RangeList a where
  -- | Try and get the `DistributionList` of the given value, throwing a
  -- `MonadException` on failure.
  rangeList :: (MonadException m, PrettyShow a) => a -> m DistributionList
  rangeList a = propagateException (prettyShow a) (rangeList' a)

  rangeList' :: (MonadException m, PrettyShow a) => a -> m DistributionList

spreadDistributions :: (Ord a) => [D.Distribution a] -> Experiment [a]
spreadDistributions [] = return []
spreadDistributions (d : ds) = from d >>= \d' -> (d' :) <$> spreadDistributions ds

instance RangeList ListValuesBase where
  rangeList' (LVBList es) = do
    exprs <- mapM range es
    return $ run $ spreadDistributions exprs
  rangeList' (LVBParen (Paren lv)) = rangeList lv

instance RangeList ListValues where
  rangeList' (LVBase lvb) = rangeList lvb
  rangeList' (MultipleValues nb b) = do
    nbd <- range nb
    bd <- range b
    return $
      run $ do
        valNum <- from nbd
        getDiceExperiment valNum bd
  rangeList' (LVFunc fi avs) = rangeFunction fi avs

rangeArgValue :: MonadException m => ArgValue -> m (D.Distribution ListInteger)
rangeArgValue (AVExpr e) = run . (LIInteger <$>) . from <$> range e
rangeArgValue (AVListValues lv) = run . (LIList <$>) . from <$> rangeList lv

rangeFunction :: (MonadException m, Ord j) => FuncInfoBase j -> [ArgValue] -> m (D.Distribution j)
rangeFunction fi exprs = do
  exprs' <- mapM rangeArgValue exprs
  let params = first (funcInfoFunc fi) <$> toList (run $ spreadDistributions exprs')
  D.fromList <$> foldAndIgnoreErrors params
  where
    foldAndIgnoreErrors = foldr (\(mv, p) mb -> mb >>= \b -> catchBot ((: []) . (,p) <$> mv) (const (return [])) >>= \v -> return (v ++ b)) (return [])
