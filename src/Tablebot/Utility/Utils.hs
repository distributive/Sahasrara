-- |
-- Module      : Tablebot.Utility.Utils
-- Description : A place for functions that don't belong anywhere else.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A place for functions to live that don't got nowhere else to live.
module Tablebot.Utility.Utils where

import Control.Monad (when)
import Data.Text (Text, filter, toLower)
import Data.Text.ICU.Char (Bool_ (Diacritic), property)
import Data.Text.ICU.Normalize (NormalizationMode (NFD), normalize)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import System.Environment (lookupEnv)
import Prelude hiding (filter)

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

-- | @standardise@ takes converts text to lowercase and removes diacritics
standardise :: Text -> Text
standardise x = filter (not . property Diacritic) normalizedText
  where
    normalizedText = normalize NFD $ toLower x

-- | Utility function to prepend a given Text to Text within a Maybe, or return
-- the empty Text.
maybeEmptyPrepend :: Text -> Maybe Text -> Text
maybeEmptyPrepend s = maybe "" (s <>)

newtype DebugString = DStr String

instance Show DebugString where
  show (DStr a) = a
