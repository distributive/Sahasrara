-- |
-- Module      : Tablebot.Plugin.DiceFunctions
-- Description : Functions, data, and type classes to deal with functions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Type classes, data, and functions that deal with functions when evaluating dice.
module Tablebot.Plugin.Dice.DiceFunctions
  ( basicFunctionsList,
    -- getBasicFunc,
    basicFunctions,
    listFunctionsList,
    -- getListFunc,
    listFunctions,
    FuncInfoBase (..),
    FuncInfo,
    ListInteger (..),
    ArgTypes (..),
  )
where

import Control.Monad.Exception (MonadException)
import Data.List (genericDrop, genericTake, sort)
import Data.Map as M (Map, fromList, keys)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Tablebot.Plugin.Exception (BotException (EvaluationException), throwBot)

-- | The limit to how big a factorial value is permitted. Notably, the factorial function doesn't operate above this limit.
factorialLimit :: Integer
factorialLimit = 50

-- Mappings for what functions are supported

-- | Mapping from function names to the functions themselves for basic functions.
basicFunctions :: MonadException m => Map Text (FuncInfo m)
basicFunctions = M.fromList $ fmap (\fi -> (funcInfoName fi, fi)) basicFunctions'

-- | The basic functions currently supported.
basicFunctionsList :: [Text]
basicFunctionsList = M.keys (basicFunctions @IO)

-- | The base details of the basic functions.
basicFunctions' :: MonadException m => [FuncInfo m]
basicFunctions' =
  sumFI :
  maximumFI :
  minimumFI :
  constructFuncInfo' "mod" (mod @Integer) (Nothing, Nothing, (== 0)) :
  constructFuncInfo' "fact" fact (Nothing, Just factorialLimit, const False) :
  (uncurry constructFuncInfo <$> [("abs", abs @Integer), ("id", id), ("neg", negate)])
  where
    fact n
      | n < 0 = 0
      | n == 0 = 1
      | n > factorialLimit = fact factorialLimit
      | otherwise = n * fact (n - 1)
    sumFI = constructFuncInfo "sum" (sum @[] @Integer)
    maximumFI = constructFuncInfo "maximum" (maximum @[] @Integer)
    minimumFI = constructFuncInfo "minimum" (minimum @[] @Integer)

-- | Mapping from function names to the functions themselves for list functions.
listFunctions :: MonadException m => Map Text (FuncInfoBase m [Integer])
listFunctions = M.fromList $ fmap (\fi -> (funcInfoName fi, fi)) listFunctions'

-- | The list functions currently supported.
listFunctionsList :: [Text]
listFunctionsList = M.keys (listFunctions @IO)

-- TODO: actually start integrating list functions

-- | The base details of the list functions.
listFunctions' :: MonadException m => [FuncInfoBase m [Integer]]
listFunctions' =
  constructFuncInfo "drop" (genericDrop @Integer @Integer) :
  constructFuncInfo "take" (genericTake @Integer @Integer) :
  (uncurry constructFuncInfo <$> [("sort", sort @Integer), ("reverse", reverse)])

data FuncInfoBase m j = FuncInfo {funcInfoName :: Text, funcInfoParameters :: [ArgTypes], funcReturnType :: ArgTypes, funcInfoFunc :: MonadException m => [ListInteger] -> m j}

type FuncInfo m = FuncInfoBase m Integer

instance Show (FuncInfoBase m j) where
  show (FuncInfo fin ft frt _) = "FuncInfo " <> unpack fin <> " " <> show ft <> " " <> show frt

constructFuncInfo :: (MonadException m, ApplyFunc m f, Returns f ~ j) => Text -> f -> FuncInfoBase m j
constructFuncInfo s f = constructFuncInfo' s f (Nothing, Nothing, const False)

constructFuncInfo' :: (MonadException m, ApplyFunc m f, Returns f ~ j) => Text -> f -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> FuncInfoBase m j
constructFuncInfo' s f bs = FuncInfo s params (last types) (applyFunc f (fromIntegral (length params)) bs)
  where
    types = getTypes f
    params = init types

data ListInteger = LIInteger Integer | LIList [Integer]
  deriving (Show, Eq, Ord)

data ArgTypes = ATInteger | ATIntegerList
  deriving (Show, Eq)

class ArgCount f where
  getArgs :: f -> Integer
  getArgs = (+ (-1)) . fromIntegral . length . getTypes
  getTypes :: f -> [ArgTypes]

instance ArgCount Integer where
  getTypes _ = [ATInteger]

instance ArgCount [Integer] where
  getTypes _ = [ATIntegerList]

instance ArgCount f => ArgCount (Integer -> f) where
  getTypes f = ATInteger : getTypes (f 1)

instance ArgCount f => ArgCount ([Integer] -> f) where
  getTypes f = ATIntegerList : getTypes (f [])

class ArgCount f => ApplyFunc m f where
  applyFunc :: (MonadException m, Returns f ~ j) => f -> Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> [ListInteger] -> m j

checkBounds :: (MonadException m) => Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> m Integer
checkBounds i (ml, mh, bs)
  | not (maybe True (i >) ml) = throwBot $ EvaluationException ("value too low for function. expected >" <> show (fromJust ml) <> ", got " <> show i) []
  | not (maybe True (i <) mh) = throwBot $ EvaluationException ("value too high for function. expected <" <> show (fromJust mh) <> ", got " <> show i) []
  | bs i = throwBot $ EvaluationException ("invalid value for function: `" <> show i ++ "`") []
  | otherwise = return i

instance {-# OVERLAPPING #-} ApplyFunc m Integer where
  applyFunc f _ _ [] = return f
  applyFunc _ args _ _ = throwBot $ EvaluationException ("incorrect number of arguments to function. expected " <> show args <> ", got more than that") []

instance {-# OVERLAPPING #-} ApplyFunc m [Integer] where
  applyFunc f _ _ [] = return f
  applyFunc _ args _ _ = throwBot $ EvaluationException ("incorrect number of arguments to function. expected " <> show args <> ", got more than that") []

instance {-# OVERLAPPABLE #-} (ApplyFunc m f) => ApplyFunc m (Integer -> f) where
  applyFunc f args _ [] = throwBot $ EvaluationException ("incorrect number of arguments to function. got " <> show dif <> ", expected " <> show args) []
    where
      dif = args - getArgs f
  applyFunc f args bs ((LIInteger x) : xs) = checkBounds x bs >>= \x' -> applyFunc (f x') args bs xs
  applyFunc _ _ _ ((LIList _) : _) = throwBot $ EvaluationException "incorrect type given to function. expected an integer, got a list" []

instance {-# OVERLAPPABLE #-} (ApplyFunc m f) => ApplyFunc m ([Integer] -> f) where
  applyFunc f args _ [] = throwBot $ EvaluationException ("incorrect number of arguments to function. got " <> show dif <> ", expected " <> show args) []
    where
      dif = args - getArgs f
  applyFunc f args bs ((LIList x) : xs) = applyFunc (f x) args bs xs
  applyFunc _ _ _ ((LIInteger _) : _) = throwBot $ EvaluationException "incorrect type given to function. expected a list, got an integer" []

type family Returns f where
  Returns Integer = Integer
  Returns [Integer] = [Integer]
  Returns (i -> j) = Returns j
