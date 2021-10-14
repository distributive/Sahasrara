-- |
-- Module      : Tablebot.Util.Error
-- Description : A place for functions that don't belong anywhere else.
-- Copyright   : (c) Anna Bruce 2021
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A place for functions to live that don't got nowhere else to live.
module Tablebot.Util.Utils where

import Control.Monad (when)
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
