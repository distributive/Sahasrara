-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceData
-- Description : Data structures for dice and other expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the basics for dice expressions and values. For an
-- overview of how they all fit together, you can view
-- `Tablebot.Plugins.Roll.Dice` to see the regex for this and brief explanations
-- for each data type.
module Tablebot.Plugins.Roll.Dice.DiceData where

import Data.Map as M (Map, fromList)
import Data.Text (Text)
import Data.Tuple (swap)
import Tablebot.Plugins.Roll.Dice.DiceFunctions (FuncInfo, FuncInfoBase)

-- | The value of an argument given to a function.
data ArgValue = AVExpr Expr | AVListValues ListValues
  deriving (Show)

-- | The type for list values.
data ListValues
  = -- | Represents `N#B`, where N is a NumBase (numbers, parentheses) and B is a Base (numbase or dice value)
    MultipleValues NumBase Base
  | -- | Represents a function call with the given arguments
    LVFunc (FuncInfoBase [Integer]) [ArgValue]
  | -- | A base ListValues value - parentheses or a list of expressions
    LVBase ListValuesBase
  deriving (Show)

-- | The type for basic list values (that can be used as is for custom dice).
--
-- A basic list value can be understood as one that is indivisible, and/or
-- atomic. They represent either a list value in parentheses, or a list of
-- expressions. Effectively what this is used for is so that these can be used
-- as dice side values.
data ListValuesBase = LVBParen (Paren ListValues) | LVBList [Expr]
  deriving (Show)

-- | The type of the top level expression. Represents one of addition,
-- subtraction, or a single term.
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
data Func = Func FuncInfo [ArgValue] | NoFunc Base
  deriving (Show)

-- | The type representing an integer value or an expression in brackets.
data NumBase = NBParen (Paren Expr) | Value Integer
  deriving (Show)

-- | Container for a parenthesised value.
newtype Paren a = Paren a
  deriving (Show)

-- | The type representing a numeric base value value or a dice value.
data Base = NBase NumBase | DiceBase Dice
  deriving (Show)

-- Dice Operations after this point

-- | The type representing a simple N sided die or a custom die, or a lazy one
-- of one of those values.
data Die = Die NumBase | CustomDie ListValuesBase | LazyDie Die deriving (Show)

-- | The type representing a number of dice equal to the `Base` value, and
-- possibly some die options.
data Dice = Dice Base Die (Maybe DieOpRecur)
  deriving (Show)

-- | The type representing one or more die options.
data DieOpRecur = DieOpRecur DieOpOption (Maybe DieOpRecur)
  deriving (Show)

-- | Some more advanced ordering options for things like `<=` and `/=`.
data AdvancedOrdering = Not AdvancedOrdering | OrderingId Ordering | And [AdvancedOrdering] | Or [AdvancedOrdering]
  deriving (Show, Eq, Ord)

-- | Compare two values according an advanced ordering.
applyCompare :: Ord a => AdvancedOrdering -> a -> a -> Bool
applyCompare (OrderingId o) a b = o == compare a b
applyCompare (And os) a b = all (\o -> applyCompare o a b) os
applyCompare (Or os) a b = any (\o -> applyCompare o a b) os
applyCompare (Not o) a b = not (applyCompare o a b)

-- | Create a mapping between a Text of the advanced ordering, and vice versa.
advancedOrderingMapping :: (Map Text AdvancedOrdering, Map AdvancedOrdering Text)
advancedOrderingMapping = (M.fromList lst, M.fromList $ swap <$> lst)
  where
    lst =
      [ ("/=", Not (OrderingId EQ)),
        ("<=", Or [OrderingId EQ, OrderingId LT]),
        (">=", Or [OrderingId EQ, OrderingId GT]),
        ("<", OrderingId LT),
        ("=", OrderingId EQ),
        (">", OrderingId GT)
      ]

-- | The type representing a die option; a reroll, a keep/drop operation, or
-- lazily performing some other die option.
data DieOpOption
  = Reroll {rerollOnce :: Bool, condition :: AdvancedOrdering, limit :: NumBase}
  | DieOpOptionKD KeepDrop LowHighWhere
  | DieOpOptionLazy DieOpOption
  deriving (Show)

-- | A type used to designate how the keep/drop option should work
data LowHighWhere = Low NumBase | High NumBase | Where AdvancedOrdering NumBase deriving (Show)

-- | Utility function to get the integer determining how many values to get
-- given a `LowHighWhere`. If the given value is `Low` or `High`, then Just the
-- NumBase contained is returned. Else, Nothing is returned.
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

-- | Utility type class for quickly promoting values.
class Converter a b where
  -- | Function that promotes an element of type `a` to an element of type `b`.
  promote :: a -> b

instance Converter ListValuesBase ListValues where
  promote = LVBase

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
