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
combineRangesBinOp :: (MonadException m, Range a, Range b, PrettyShow a, PrettyShow b) => (Integer -> Integer -> Integer) -> a -> b -> m Distribution
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
    d'' <- dropWhereDistribution (== 0) d'
    combineDistributionsBinOp div d d''

instance Range Negation where
  range' (Neg t) = mapOverValue negate <$> range t
  range' (NoNeg t) = range t

instance Range Expo where
  range' (NoExpo t) = range t
  range' (Expo t e) = do
    d <- range t
    d' <- range e
    -- having negative values is disallowed
    d'' <- dropWhereDistribution (< 0) d'
    combineDistributionsBinOp (^) d d''

instance Range Func where
  range' (NoFunc t) = range t
  range' f@(Func _ _) = evaluationException "tried to find range of function, which is currently unsupported" [prettyShow f]

instance Range NumBase where
  range' (Value i) = toDistribution [(i, 1)]
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
    vcs <- sequence $ (\(hv, p) -> toDistribution ((,1 / fromIntegral hv) <$> [1 .. hv]) <&> (,p)) <$> fromDistribution nbr
    -- then condense that into a single distribution
    mergeWeightedDistributions vcs
  range' (CustomDie (LVBList es)) = do
    -- get the distribution for each value in the custom die
    exprs <- mapM range es
    let l = genericLength es
    -- then merge all the distributions. each distribution is equally likely to
    -- come up.
    mergeWeightedDistributions ((,1 / l) <$> exprs)
  range' cd@(CustomDie _) = evaluationException "tried to find range of complex custom die" [prettyShow cd]

instance Range Dice where
  range' (Dice b d mdor) = do
    b' <- range b
    d' <- range d
    dcs <- rangeDieOp d' mdor [(b', d', 1)] >>= sequence . (fromCountAndDie <$>)
    mergeWeightedDistributions dcs

-- | Aliased type to represent a singular instance of (number of dice,
-- distribution of a die, the probability of this occuring).
type DiceCollection = (Distribution, Distribution, Rational)

-- | From a `DiceCollection`, get a distribution and the probability of that
-- distribution.
fromCountAndDie :: MonadException m => DiceCollection -> m (Distribution, Rational)
fromCountAndDie (c, d, r) = do
  mwd <- sequence $ do
    (i, p) <- fromDistribution c
    if i < 1
      then [toDistribution [(0, 1)] <&> (,p)]
      else do
        let v = catchEmptyDistribution $ Prelude.foldr1 (\a b -> a >>= \a' -> b >>= \b' -> combineDistributionsBinOp (+) a' b') (genericTake i (repeat (return d)))
        [v <&> (,p)]
  mwd' <- catchEmptyDistribution $ mergeWeightedDistributions mwd
  return (mwd', r)

-- | Step by step apply `rangeDieOp'`, returning the current list of
-- `DiceCollection`s when `Nothing` is encountered.
rangeDieOp :: (MonadException m) => Distribution -> Maybe DieOpRecur -> [DiceCollection] -> m [DiceCollection]
rangeDieOp _ Nothing ds = return ds
rangeDieOp die (Just (DieOpRecur doo mdor)) ds = rangeDieOp' die doo ds >>= rangeDieOp die mdor

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
          (limitValue, limitProbability) <- fromDistribution limd
          return
            ( -- get the new die distribution (only relevant on infinite
              -- rerolls). if the die distribution is invalid (no values), an
              -- exception is thrown here, as early as possible.
              -- then, transform the given dice collections
              die' limitValue >>= transformDiceCollections limitValue limitProbability
            )
      )
  where
    die' limitValue
      | rro = return die
      | otherwise = do
        d <- dropWhereDistribution (\i -> not $ applyCompare cond i limitValue) die
        if nullDistribution d
          then evaluationException "cannot reroll die infinitely; range is incorrect" []
          else return d

    -- Go through all the dice values and conditionally perform the reroll.
    conditionallyReroll dieDistribution limitValue newDie = sequence $ do
      (dieV, dieP) <- fromDistribution dieDistribution
      if applyCompare cond dieV limitValue
        then [return (newDie, dieP)]
        else [toDistribution [(dieV, 1)] <&> (,dieP)]
    transformDiceCollections limitValue limitProbability newDie =
      sequence
        ( do
            -- for each dice collection in the list, perform  the
            -- below.
            (c, dieDistribution, cp) <- ds
            -- return the list of dice collections, sequencing as
            -- needed
            return $
              conditionallyReroll dieDistribution limitValue newDie
                >>= ( mergeWeightedDistributions
                        >=> ( \mwd -> return (c, mwd, cp * limitProbability)
                            )
                    )
        )

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
      | otherwise = repeatedM m (i - 1) d >>= \d' -> combineDistributionsBinOp m d d'
    repeatedMinimum = repeatedM min
    repeatedMaximum = repeatedM max
    chooseType Keep (High _) = return repeatedMaximum
    chooseType Keep (Low _) = return repeatedMinimum
    chooseType Drop (Low _) = return repeatedMaximum
    chooseType Drop (High _) = return repeatedMinimum
    chooseType _ _ = evaluationException "keep/drop where is unsupported" []
