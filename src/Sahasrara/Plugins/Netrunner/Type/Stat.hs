-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Stat type.
module Sahasrara.Plugins.Netrunner.Type.Stat where

import Data.Aeson (FromJSON, Value (..), parseJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sahasrara.Utility

-- | @Stat@ represents values that are either a numeric value or a variable (usually X)
data Stat = Var Text | Val Int deriving (Eq, Show, Generic)

statToText :: Stat -> Text
statToText (Var x) = x
statToText (Val x) = intToText x

isVar :: Stat -> Bool
isVar (Var _) = True
isVar _ = False

isVal :: Stat -> Bool
isVal (Val _) = True
isVal _ = False

instance FromJSON Stat where
  parseJSON (String var) = Var <$> pure var
  parseJSON (Number val) =
    let v = fromMaybe 0 $ toBoundedInteger val
     in if v < 0
          then Var <$> pure "X"
          else pure $ Val v
  parseJSON v = typeMismatch "Stat" v
