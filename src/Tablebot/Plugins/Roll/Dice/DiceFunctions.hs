-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceFunctions
-- Description : Functions, data, and type classes to deal with functions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Type classes, data, and functions that deal with functions when evaluating
-- dice.
module Tablebot.Plugins.Roll.Dice.DiceFunctions
  ( integerFunctionsList,
    integerFunctions,
    listFunctionsList,
    listFunctions,
    FuncInfoBase (..),
    FuncInfo,
    ListInteger (..),
    ArgType (..),
  )
where

import Control.Monad.Exception (MonadException)
import Data.List (genericDrop, genericLength, genericTake, sort)
import Data.Map as M (Map, fromList, keys)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Tablebot.Utility.Exception (BotException (EvaluationException), throwBot)

-- | The limit to how big a factorial value is permitted. Notably, the factorial
-- function doesn't operate above this limit.
factorialLimit :: Integer
factorialLimit = 50

-- Mappings for what functions are supported

-- | Mapping from function names to the functions themselves for integer
-- functions.
integerFunctions :: Map Text FuncInfo
integerFunctions = M.fromList $ fmap (\fi -> (funcInfoName fi, fi)) integerFunctions'

-- | The names of the integer functions currently supported.
integerFunctionsList :: [Text]
integerFunctionsList = M.keys integerFunctions

-- | The base details of the integer functions, containing all the information
-- for each function that returns an integer.
integerFunctions' :: [FuncInfo]
integerFunctions' =
  funcInfoIndex :
  constructFuncInfo "length" (genericLength @Integer @Integer) :
  constructFuncInfo "sum" (sum @[] @Integer) :
  constructFuncInfo "max" (max @Integer) :
  constructFuncInfo "min" (min @Integer) :
  constructFuncInfo "maximum" (maximum @[] @Integer) :
  constructFuncInfo "minimum" (minimum @[] @Integer) :
  constructFuncInfo' "mod" (mod @Integer) (Nothing, Nothing, (== 0)) :
  constructFuncInfo' "fact" fact (Nothing, Just factorialLimit, const False) :
  (uncurry constructFuncInfo <$> [("abs", abs @Integer), ("id", id), ("neg", negate)])
  where
    fact n
      | n < 0 = 0
      | n == 0 = 1
      | n > factorialLimit = fact factorialLimit
      | otherwise = n * fact (n - 1)

-- | Mapping from function names to the functions themselves for list functions.
listFunctions :: Map Text (FuncInfoBase [Integer])
listFunctions = M.fromList $ fmap (\fi -> (funcInfoName fi, fi)) listFunctions'

-- | The names of the list functions currently supported.
listFunctionsList :: [Text]
listFunctionsList = M.keys listFunctions

-- | The base details of the list functions, containing all the information for
-- each function that returns an integer.
listFunctions' :: [FuncInfoBase [Integer]]
listFunctions' =
  constructFuncInfo "concat" (++) :
  constructFuncInfo "between" between :
  constructFuncInfo "drop" (genericDrop @Integer) :
  constructFuncInfo "take" (genericTake @Integer) :
  (uncurry constructFuncInfo <$> [("sort", sort), ("reverse", reverse)])
  where
    between i i' = let (mi, ma, rev) = (min i i', max i i', if i > i' then reverse else id) in rev [mi .. ma]

-- | The `FuncInfo` of the function that indexes into a list.
funcInfoIndex :: FuncInfo
funcInfoIndex = FuncInfo "index" [ATInteger, ATIntegerList] ATInteger fiIndex
  where
    fiIndex (LIInteger i : [LIList is])
      | i < 0 || i >= genericLength is = throwBot $ EvaluationException ("index out of range: " ++ show i) []
      | otherwise = return (is !! fromInteger i)
    fiIndex is = throwBot $ EvaluationException ("incorrect number of arguments. expected 2, got " ++ show (length is)) []

-- | A data structure to contain the information about a given function,
-- including types, the function name, and the function itself.
data FuncInfoBase j = FuncInfo {funcInfoName :: Text, funcInfoParameters :: [ArgType], funcReturnType :: ArgType, funcInfoFunc :: forall m. (MonadException m) => [ListInteger] -> m j}

type FuncInfo = FuncInfoBase Integer

instance Show (FuncInfoBase j) where
  show (FuncInfo fin ft frt _) = "FuncInfo " <> unpack fin <> " " <> show ft <> " " <> show frt

-- | A simple way to construct a function that returns a value j, and has no
-- constraints on the given values.
constructFuncInfo :: forall j f. (ApplyFunc f, Returns f ~ j) => Text -> f -> FuncInfoBase j
constructFuncInfo s f = constructFuncInfo' s f (Nothing, Nothing, const False)

-- | Construct a function info when given optional constraints.
constructFuncInfo' :: forall j f. (ApplyFunc f, Returns f ~ j) => Text -> f -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> FuncInfoBase j
constructFuncInfo' s f bs = FuncInfo s params (last types) (applyFunc f (fromIntegral (length params)) bs)
  where
    types = getTypes f
    params = init types

-- | Some evaluated values, either an integer or a list of values with their
-- representations.
data ListInteger = LIInteger Integer | LIList [Integer]
  deriving (Show, Eq, Ord)

-- | Values representing argument types.
data ArgType = ATInteger | ATIntegerList
  deriving (Show, Eq)

-- | A type class for counting the amount of arguments of a function, and their
-- types. Only supports integers and integer lists currently.
class ArgCount f where
  -- | Get the number of arguments to a function.
  getArgs :: f -> Integer
  getArgs = (+ (-1)) . fromIntegral . length . getTypes

  -- | Get the types of arguments to a function.
  getTypes :: f -> [ArgType]

instance ArgCount Integer where
  getTypes _ = [ATInteger]

instance ArgCount [Integer] where
  getTypes _ = [ATIntegerList]

instance ArgCount f => ArgCount (Integer -> f) where
  getTypes f = ATInteger : getTypes (f 1)

instance ArgCount f => ArgCount ([Integer] -> f) where
  getTypes f = ATIntegerList : getTypes (f [1])

-- | Type class which represents applying a function f to some inputs when given
-- the bounds for the function and some number of inputs.
--
-- If the number of inputs is incorrect or the value given out of the range, an
-- exception is thrown.
class ArgCount f => ApplyFunc f where
  -- | Takes a function, the number of arguments in the function overall, bounds
  -- on integer values to the function, and a list of `ListInteger`s (which are
  -- either a list of integers or an integer), and returns a wrapped `j` value,
  -- which is a value that the function originally returns.
  --
  -- The bounds represent the exclusive lower bound, the exclusive upper bound,
  -- and an arbitrary function which results in an exception when it is true;
  -- say, with division when you want to deny just 0 as a value.
  applyFunc :: forall m j. (MonadException m, Returns f ~ j) => f -> Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> [ListInteger] -> m j

-- | Check whether a given value is within the given bounds.
checkBounds :: (MonadException m) => Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> m Integer
checkBounds i (ml, mh, bs)
  | not (maybe True (i >) ml) = throwBot $ EvaluationException ("value too low for function. expected >" <> show (fromJust ml) <> ", got " <> show i) []
  | not (maybe True (i <) mh) = throwBot $ EvaluationException ("value too high for function. expected <" <> show (fromJust mh) <> ", got " <> show i) []
  | bs i = throwBot $ EvaluationException ("invalid value for function: `" <> show i ++ "`") []
  | otherwise = return i

-- This is one of two base cases for applyFunc. This is the case where the
-- return value is an integer. As it is the return value, no arguments are
-- accepted.
instance {-# OVERLAPPING #-} ApplyFunc Integer where
  applyFunc f _ _ [] = return f
  applyFunc _ args _ _ = throwBot $ EvaluationException ("incorrect number of arguments to function. expected " <> show args <> ", got more than that") []

-- This is one of two base cases for applyFunc. This is the case where the
-- return value is a list of integers. As it is the return value, no arguments
-- are  accepted.
instance {-# OVERLAPPING #-} ApplyFunc [Integer] where
  applyFunc f _ _ [] = return f
  applyFunc _ args _ _ = throwBot $ EvaluationException ("incorrect number of arguments to function. expected " <> show args <> ", got more than that") []

-- This is one of two recursive cases for applyFunc. This is the case where the
-- argument value is an integer. If there are no arguments or the argument is
-- of the wrong type, an exception is thrown.
instance {-# OVERLAPPABLE #-} (ApplyFunc f) => ApplyFunc (Integer -> f) where
  applyFunc f args _ [] = throwBot $ EvaluationException ("incorrect number of arguments to function. got " <> show dif <> ", expected " <> show args) []
    where
      dif = args - getArgs f
  applyFunc f args bs ((LIInteger x) : xs) = checkBounds x bs >>= \x' -> applyFunc (f x') args bs xs
  applyFunc _ _ _ (_ : _) = throwBot $ EvaluationException "incorrect type given to function. expected an integer, got a list" []

-- This is one of two recursive cases for applyFunc. This is the case where the
-- argument value is a list of integers. If there are no arguments or the
-- argument is of the wrong type, an exception is thrown.
instance {-# OVERLAPPABLE #-} (ApplyFunc f) => ApplyFunc ([Integer] -> f) where
  applyFunc f args _ [] = throwBot $ EvaluationException ("incorrect number of arguments to function. got " <> show dif <> ", expected " <> show args) []
    where
      dif = args - getArgs f
  applyFunc f args bs ((LIList x) : xs) = applyFunc (f x) args bs xs
  applyFunc _ _ _ (_ : _) = throwBot $ EvaluationException "incorrect type given to function. expected a list, got an integer" []

-- | Simple type family that gets the return type of whatever function or value
-- is given
type family Returns f where
  Returns (i -> j) = Returns j
  Returns j = j
