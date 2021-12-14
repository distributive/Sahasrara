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
  ( supportedFunctionsList,
    supportedFunctions,
    FuncInfo (..),
    ListInteger (..),
    ArgTypes (..),
  )
where

import Control.Monad.Exception (MonadException)
import Data.Map as M (Map, fromList, keys)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Tablebot.Plugin.Exception (BotException (EvaluationException), throwBot)

-- | The limit to how big a factorial value is permitted. Notably, the factorial function doesn't operate above this limit.
factorialLimit :: Integer
factorialLimit = 50

-- Mappings for what functions are supported

-- | Mapping from function names to the functions themselves.
supportedFunctions :: MonadException m => Map Text (FuncInfo m)
supportedFunctions = M.fromList $ fmap (\fi -> (funcInfoName fi, fi)) supportedFunctions'

supportedFunctions' :: MonadException m => [FuncInfo m]
supportedFunctions' =
  sumFI :
  maximumFI :
  minimumFI :
  constructFuncInfo' "mod" (mod @Integer) (Nothing, Nothing, (== 0)) :
  constructFuncInfo' "fact" fact (Nothing, Just factorialLimit, const False) : (uncurry constructFuncInfo <$> [("abs", abs @Integer), ("id", id), ("neg", negate)])
  where
    fact n
      | n < 0 = 0
      | n == 0 = 1
      | n > factorialLimit = fact factorialLimit
      | otherwise = n * fact (n - 1)
    sumFI = constructFuncInfo' "sum" (sum @[] @Integer) (Nothing, Nothing, const False)
    maximumFI = constructFuncInfo' "maximum" (maximum @[] @Integer) (Nothing, Nothing, const False)
    minimumFI = constructFuncInfo' "minimum" (minimum @[] @Integer) (Nothing, Nothing, const False)

-- | The functions currently supported.
supportedFunctionsList :: [Text]
supportedFunctionsList = M.keys (supportedFunctions @IO)

data FuncInfo m = FuncInfo {funcInfoName :: Text, funcTypes :: [ArgTypes], funcInfoFunc :: MonadException m => [ListInteger] -> m Integer}

instance Show (FuncInfo m) where
  show (FuncInfo fin ft _) = "FuncInfo " <> unpack fin <> " " <> show ft

constructFuncInfo :: (MonadException m, ApplyFunc m f) => Text -> f -> FuncInfo m
constructFuncInfo s f = constructFuncInfo' s f (Nothing, Nothing, const False)

constructFuncInfo' :: (MonadException m, ApplyFunc m f) => Text -> f -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> FuncInfo m
constructFuncInfo' s f bs = FuncInfo s types (applyFunc f (fromIntegral (length types)) bs)
  where
    types = init $ getTypes f

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

instance ArgCount f => ArgCount (Integer -> f) where
  getTypes f = ATInteger : getTypes (f 1)

instance ArgCount f => ArgCount ([Integer] -> f) where
  getTypes f = ATIntegerList : getTypes (f [])

class ArgCount f => ApplyFunc m f where
  applyFunc :: (MonadException m) => f -> Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> [ListInteger] -> m Integer

checkBounds :: (MonadException m) => Integer -> (Maybe Integer, Maybe Integer, Integer -> Bool) -> m Integer
checkBounds i (ml, mh, bs)
  | not (maybe True (i >) ml) = throwBot $ EvaluationException ("value too low for function. expected >" <> show (fromJust ml) <> ", got " <> show i) []
  | not (maybe True (i <) mh) = throwBot $ EvaluationException ("value too high for function. expected <" <> show (fromJust mh) <> ", got " <> show i) []
  | bs i = throwBot $ EvaluationException ("invalid value for function: `" <> show i ++ "`") []
  | otherwise = return i

instance {-# OVERLAPPING #-} ApplyFunc m Integer where
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
