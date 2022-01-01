{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : Tablebot.Plugin.DiceStats
-- Description : Get statistics on particular expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin generates statistics based on the values of dice in given expressions
module Tablebot.Plugin.DiceStats where

import Data.Map as M ((!))
import Data.Maybe (fromMaybe)
import Data.Set as S (Set, fromList, singleton, toList, unions)
import Safe.Foldable (maximumMay, minimumMay)
import Tablebot.Plugin.Dice

-- {-
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
instance Range Func where
  range' (Func s n) = S.fromList $ (supportedFunctions M.! s) <$> range n
  maxVal (Func "id" n) = maxVal n
  maxVal f = maximum (range f)
  minVal (Func "id" n) = minVal n
  minVal f = minimum (range f)

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

-- -}
