-- |
-- Module      : Tablebot.Plugin.DiceData
-- Description : Data structures for dice and other expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the basics for dice expressions and values.
module Tablebot.Plugin.Dice.DiceData where

import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty as NE (fromList)
import Data.Map as M (Map, fromList)
import Data.Set as S (Set, map)
import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Tablebot.Plugin.Dice.DiceFunctions (FuncInfo)
import Tablebot.Plugin.Types (Parser)
import Text.Megaparsec (failure)
import Text.Megaparsec.Error (ErrorItem (Tokens))

failure' :: Text -> Set Text -> Parser a
failure' s ss = failure (Just $ Tokens $ NE.fromList $ unpack s) (S.map (Tokens . NE.fromList . unpack) ss)

data ListValues = NoList Expr | MultipleValues NumBase Base | LVList [Expr]
  deriving (Show)

-- | The type of the top level expression. Represents one of addition, subtraction, or a
-- single term.
data Expr = Add Term Expr | Sub Term Expr | NoExpr Term
  deriving (Show)

-- | The type representing multiplication, division, or a single negated term.
data Term = Multi Negation Term | Div Negation Term | NoTerm Negation
  deriving (Show)

-- | The type representing a possibly negated value.
data Negation = Neg Expo | NoNeg Expo
  deriving (Show)

-- | The type representing a value with exponentials.
data Expo = Expo Func Expo | NoExpo Func
  deriving (Show)

-- | The type representing a single function application, or a base item.
data Func = Func (FuncInfo IO) [ListValues] | NoFunc Base
  deriving (Show)

-- | The type representing an integer value or an expression in brackets.
data NumBase = Paren Expr | Value Integer
  deriving (Show)

-- | The type representing a numeric base value value or a dice value.
data Base = NBase NumBase | DiceBase Dice
  deriving (Show)

-- Dice Operations after this point

-- TODO: create a lazy and a repeated eval die so that sometimes dice quantities are eval'd
-- each time and sometimes are eval'd once and then that value is used

-- | The type representing a simple N sided die or a custom die.
data Die = Die NumBase | CustomDie [Expr] | LazyDie Die deriving (Show)

-- | The type representing a number of dice equal to the `Base` value, and possibly some
-- die options.
data Dice = Dice Base Die (Maybe DieOpRecur)
  deriving (Show)

-- | The type representing one or more die options.
data DieOpRecur = DieOpRecur DieOpOption (Maybe DieOpRecur)
  deriving (Show)

data AdvancedOrdering = Not AdvancedOrdering | OrderingId Ordering | And [AdvancedOrdering] | Or [AdvancedOrdering]
  deriving (Show, Eq, Ord)

applyCompare :: Ord a => AdvancedOrdering -> a -> a -> Bool
applyCompare (OrderingId o) a b = o == compare a b
applyCompare (And os) a b = all (\o -> applyCompare o a b) os
applyCompare (Or os) a b = any (\o -> applyCompare o a b) os
applyCompare (Not o) a b = not (applyCompare o a b)

advancedOrderingMapping :: (IsString a, Ord a) => (Map a AdvancedOrdering, Map AdvancedOrdering a)
advancedOrderingMapping = (M.fromList lst, M.fromList $ swap <$> lst)
  where
    lst =
      fmap
        (first fromString)
        [ ("/=", Not (OrderingId EQ)),
          ("<=", Or [OrderingId EQ, OrderingId LT]),
          (">=", Or [OrderingId EQ, OrderingId GT]),
          ("<", OrderingId LT),
          ("=", OrderingId EQ),
          (">", OrderingId GT)
        ]

-- | The type representing a die option.
data DieOpOption
  = Reroll {rerollOnce :: Bool, condition :: AdvancedOrdering, limit :: NumBase}
  | DieOpOptionKD KeepDrop LowHighWhere
  | DieOpOptionLazy DieOpOption
  deriving (Show)

-- | A type used to designate how the keep/drop option should work
data LowHighWhere = Low NumBase | High NumBase | Where AdvancedOrdering NumBase deriving (Show)

-- | Utility function to get the integer determining how many values to get given a
-- `LowHighWhere`. If the given value is `Low` or `High`, then Just the NumBase contained
-- is returned. Else, Nothing is returned.
getValueLowHigh :: LowHighWhere -> Maybe NumBase
getValueLowHigh (Low i) = Just i
getValueLowHigh (High i) = Just i
getValueLowHigh (Where _ _) = Nothing

-- | Returns whether the given `LowHighWhere` is `Low` or not.
isLow :: LowHighWhere -> Bool
isLow (Low _) = True
isLow _ = False

-- | Utility value for whether to keep or drop values.
data KeepDrop = Keep | Drop deriving (Show, Eq)

-- | Utility type class for quickly promoting values
class Converter a b where
  -- | Function that promotes an element of type `a` to an element of type `b`.
  promote :: a -> b

instance (Converter a Expr) => Converter a ListValues where
  promote = NoList . promote

instance (Converter a Term) => Converter a Expr where
  promote = NoExpr . promote

instance (Converter a Negation) => Converter a Term where
  promote = NoTerm . promote

instance (Converter a Expo) => Converter a Negation where
  promote = NoNeg . promote

instance (Converter a Func) => Converter a Expo where
  promote = NoExpo . promote

instance (Converter a Base) => Converter a Func where
  promote = NoFunc . promote

instance Converter Integer Base where
  promote = NBase . promote

instance Converter NumBase Base where
  promote = NBase

instance Converter Integer NumBase where
  promote = Value

instance Converter Dice Base where
  promote = DiceBase

instance Converter Die Base where
  promote d = promote $ Dice (promote (1 :: Integer)) d Nothing
