-- |
-- Module      : Tablebot.Plugin.Util
-- Description : A place for functions that don't belong anywhere else.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A place for functions to live that don't got nowhere else to live.
module Tablebot.Plugin.Utils where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import System.Environment (lookupEnv)

isDebug :: IO Bool
isDebug = do
  d <- lookupEnv "DEBUG"
  return $ justDebug d
  where
    justDebug (Just "True") = True
    justDebug (Just "true") = True
    justDebug (Just "1") = True
    justDebug _ = False

debugPrint :: Show a => a -> IO ()
debugPrint a = do
  d <- isDebug
  when d $ print a

intToText :: Integral a => a -> Text
intToText = toStrict . toLazyText . decimal
