module Tablebot.Plugin.Dice.DiceFunctions
  ( supportedFunctionsList,
    supportedFunctions,
    getFunc,
    FuncInfo (..),
    ListInteger (..),
    ArgTypes (..),
  )
where

import Control.Monad.Exception (MonadException)
import Data.Map as M (Map, findWithDefault, fromList, keys, map)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Tablebot.Plugin.Discord (Format (..), formatText)
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

-- | n > factorialLimit = throwBot $ EvaluationException ("tried to evaluate a factorial with input number greater than the limit (" ++ formatInput Code factorialLimit ++ "): `" ++ formatInput Code n ++ "`") []

-- | The functions currently supported.
supportedFunctionsList :: [Text]
supportedFunctionsList = M.keys (supportedFunctions @IO)

---- | Functions that looks up the given function name in the map, and will either throw an
---- error or return the function (wrapped inside the given monad)
getFunc :: MonadException m => Text -> [ListInteger] -> m Integer
getFunc s is = do
  fi <- M.findWithDefault (throwBot $ EvaluationException ("could not find function " ++ formatText Code (unpack s)) []) s (M.map (return . funcInfoFunc) supportedFunctions)
  fi is

data FuncInfo m = FuncInfo {funcInfoName :: Text, funcTypes :: [ArgTypes], funcInfoFunc :: MonadException m => [ListInteger] -> m Integer}

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

-- instance {-# OVERLAPPING #-} MonadException m => ApplyFunc m (m Integer) where
--   applyFunc f _ [] = f
--   applyFunc _ i _ = throwBot $ EvaluationException ("incorrect number of arguments to function. expected " <> show i <> ", got more than that") []

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
