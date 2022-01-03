-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStats
-- Description : Get statistics on particular expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin generates statistics based on the values of dice in given expressions
module Tablebot.Plugins.Roll.Dice.DiceStats where

import Control.Monad.Exception (MonadException)
import Data.List (genericTake)
import Data.Map as M
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceEval (evaluationException)
import Tablebot.Plugins.Roll.Dice.DiceFunctions
import Tablebot.Plugins.Roll.Dice.DiceStatsBase

-- combineDistributionsBinOp'  :: (Monad m) => (Integer -> Integer -> Integer) -> m Distribution -> m Distribution -> m Distribution
-- combineDistributionsBinOp' f m m' = do
--     d <- m
--     d' <- m'
--     return $ combineDistributionsBinOp f d d'

combineRangesBinOp :: (MonadException m, Range a, Range b) => (Integer -> Integer -> Integer) -> a -> b -> m Distribution
combineRangesBinOp f a b = do
  d <- range a
  d' <- range b
  return $ combineDistributionsBinOp f d d'

class Range a where
  range :: MonadException m => a -> m Distribution

-- maxValue :: MonadException m => a -> m Integer
-- minValue :: MonadException m => a -> m Integer

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
    return $ combineDistributionsBinOp div d (dropWhereDistribution (== 0) d')

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
    return $ combineDistributionsBinOp (^) d (dropWhereDistribution (>= 0) d')

instance Range Func where
  range (NoFunc t) = range t
  range f@(Func _ _) = evaluationException "tried to find range of function" [prettyShow f]

instance Range NumBase where
  range (Value i) = return $ toDistribution [(i, 1)]
  range (NBParen (Paren e)) = range e

instance Range Base where
  range (NBase nb) = range nb
  range (DiceBase d) = range d

instance Range Die where
  range (LazyDie d) = range d
  range (Die nb) = do
    nbr <- range nb
    let vcs = (\(hv, p) -> (toDistribution ((,1 / fromIntegral hv) <$> [1 .. hv]), p)) <$> fromDistribution nbr
    return $ mergeWeightedDistributions vcs
  range (CustomDie (LVBList es)) = do
    exprs <- mapM range es
    let l = fromIntegral $ length es
    return $ mergeWeightedDistributions ((,1 / l) <$> exprs)
  range cd@(CustomDie _) = evaluationException "tried to find range of complex custom die" [prettyShow cd]

instance Range Dice where
  range dice@(Dice b d mdor) = rangeDieOp dice

rangeDieOp :: (MonadException m) => Dice -> m Distribution
rangeDieOp (Dice b d Nothing) = do
  bDis <- range b
  dDis <- range d
  let endDises = do
        (i, p) <- fromDistribution bDis
        if i < 1
          then []
          else do
            let v = Prelude.foldr1 (combineDistributionsBinOp (+)) (genericTake i (repeat dDis))
            [(v, p)]
  return $ mergeWeightedDistributions endDises
rangeDieOp d = evaluationException "die modifiers are unimplemented" [prettyShow d]

-- rangeDieOpHelpKD :: (MonadException m) => KeepDrop -> LowHighWhere -> (Distribution , Distribution ) -> m (Distribution , Distribution )
-- rangeDieOpHelpKD kd (Where cmp i) ds@(counts,values) = do
--   iDis <- range i
--   let ds = do
--         (i,r) <- fromDistribution iDis
--         if doesActivate i then return () else r
--   r
--   where
--     vs = fromDistribution values
--     doesActivate i' = any (\(i'',_) -> applyCompare cmp i'' i') vs

{-

--- Finding the range of an expression.

-- TODO: make range return all possible values, repeated the number of times they would be
-- present (so it can be used for statistics)

-- | Type class to find the range and bounds of a given value.
class Range a where
  range :: a -> [Integer]
  range = toList . range'
  range' :: a -> Set Integer
  maxVal :: a -> Integer
  minVal :: a -> Integer

instance Range Expr where
  range' (Add t e) = S.fromList $ ((+) <$> range t) <*> range e
  range' (Sub t e) = S.fromList $ ((-) <$> range t) <*> range e
  range' (NoExpr t) = range' t
  maxVal (Add t e) = maxVal t + maxVal e
  maxVal (Sub t e) = maxVal t - minVal e
  maxVal (NoExpr t) = maxVal t
  minVal (Add t e) = minVal t + minVal e
  minVal (Sub t e) = minVal t - maxVal e
  minVal (NoExpr t) = minVal t

instance Range Term where
  range' (Multi f t) = S.fromList $ ((*) <$> range f) <*> range t
  range' (Div f t) = S.fromList $ (div <$> range f) <*> filter (/= 0) (range t)
  range' (NoTerm f) = range' f
  maxVal (Multi f t) = maxVal f * maxVal t
  maxVal (Div f t) = maxVal f `div` minVal t
  maxVal (NoTerm f) = maxVal f
  minVal (Multi f t) = minVal f * minVal t
  minVal (Div f t) = minVal f `div` maxVal t
  minVal (NoTerm f) = minVal f

-- NOTE: this is unsafe since the function requested may not be defined
-- if using the dice parser functions, it'll be safe, but for all other uses, beware
-- instance Range Func where
--   range' (Func s n) = S.fromList $ (supportedFunctions M.! s) <$> range n
--   maxVal (Func "id" n) = maxVal n
--   maxVal f = maximum (range f)
--   minVal (Func "id" n) = minVal n
--   minVal f = minimum (range f)

instance Range Negation where
  range' (Neg expo) = S.fromList $ negate <$> range expo
  range' (NoNeg expo) = range' expo
  maxVal (NoNeg expo) = maxVal expo
  maxVal (Neg expo) = negate $ minVal expo
  minVal (NoNeg expo) = minVal expo
  minVal (Neg expo) = negate $ maxVal expo

instance Range Expo where
  range' (NoExpo b) = range' b
  range' (Expo b expo) = S.fromList $ ((^) <$> range b) <*> range expo
  maxVal (NoExpo b) = maxVal b
  maxVal (Expo b expo) = maxVal b ^ maxVal expo
  minVal (NoExpo b) = minVal b
  minVal (Expo b expo) = minVal b ^ minVal expo

instance Range NumBase where
  range' (Value i) = singleton i
  range' (Paren e) = range' e
  maxVal (Value i) = i
  maxVal (Paren e) = maxVal e
  minVal (Value i) = i
  minVal (Paren e) = minVal e

instance Range Base where
  range' (NBase nb) = range' nb
  range' (DiceBase dop) = range' dop
  maxVal (NBase nb) = maxVal nb
  maxVal (DiceBase dop) = maxVal dop
  minVal (NBase nb) = minVal nb
  minVal (DiceBase dop) = minVal dop

instance Range Die where
  -- range' (CustomDie is) = S.fromList is
  range' (Die b) = S.fromList [1 .. (maxVal b)]

  -- maxVal (CustomDie is) = maximum is
  maxVal (Die b) = maxVal b

  -- minVal (CustomDie is) = minimum is
  minVal (Die _) = 1

-- TODO: check this more
instance Range Dice

{-
  range' d = S.unions $ fmap foldF counts
    where
      (counts, dr) = diceVals d
      foldF' i js
        | i < 1 = []
        | i == 1 = js
        | otherwise = ((+) <$> dr) <*> foldF' (i - 1) js
      foldF i = S.fromList $ foldF' i dr
  maxVal d
    | mxdr < 0 = fromMaybe 0 (minimumMay counts) * mxdr
    | otherwise = fromMaybe 0 (maximumMay counts) * mxdr
    where
      (counts, dr) = diceVals d
      mxdr = fromMaybe 0 $ maximumMay dr
  minVal d
    | mndr < 0 = fromMaybe 0 (maximumMay counts) * mndr
    | otherwise = fromMaybe 0 (minimumMay counts) * mndr
    where
      (counts, dr) = diceVals d
      mndr = fromMaybe 0 $ minimumMay dr

type DieRange = [Integer]

-- the tuple is the range of the number of dice, the current die range, and the total die range possible with the dice being used

-- | Applies a given die operation to the current die ranges. The tuple given and returned
-- represents the number of dice, the current range of the die, and the base die range.
applyDieOpVal :: DieOpOption -> ([Integer], DieRange, DieRange) -> ([Integer], DieRange, DieRange)
applyDieOpVal (Reroll ro c l) t@(is, cdr, dr)
  | any boolF cdr = (is, applyBoolF dr, dr)
  | otherwise = t
  where
    boolF i' = compare i' l == c
    applyBoolF = if ro then id else filter (not . boolF)
applyDieOpVal (DieOpOptionKD kd (Where o i)) (is, cdr, dr)
  | any boolF cdr = ([0 .. maximum is], filter boolF cdr, dr)
  | otherwise = (is, cdr, dr)
  where
    boolF i' = (if kd == Keep then id else not) $ compare i' i == o
applyDieOpVal (DieOpOptionKD kd lh) (is, cdr, dr) = (f (getValueLowHigh lh) <$> is, cdr, dr)
  where
    f (Just i) i' = if kd == Keep then min i i' else max 0 (i' - i)
    f Nothing i' = i'

-- | Get the number of dice and the die range of a given set of dice.
diceVals :: Dice -> ([Integer], DieRange)
diceVals (Dice b d mdor) = (filter (>= 0) counts, dr)
  where
    (counts, dr, _) = diceVals' mdor (filter (>= 0) (range b), dieVals, dieVals)
    dieVals = range d

-- | Helper function to iterate through all the `DieOpOption`s for a give set of dice.
diceVals' :: Maybe DieOpRecur -> ([Integer], DieRange, DieRange) -> ([Integer], DieRange, DieRange)
diceVals' Nothing t = t
diceVals' (Just (DieOpRecur doo mdor)) t = diceVals' mdor (applyDieOpVal doo t)

-}
-}
