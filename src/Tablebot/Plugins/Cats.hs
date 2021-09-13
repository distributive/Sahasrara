-- |
-- Module      : Tablebot.Plugins.Cats
-- Description : A very simple example plugin.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which just responds with a cat photo to a .cat call
module Tablebot.Plugins.Cats (catPlugin) where

import Control.Monad (MonadPlus (mzero))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Text (Text)
import Data.Vector ((!))
import GHC.Exception (getCallStack)
import Network.HTTP.Client.Conduit (withManager)
import Network.HTTP.Conduit
  ( Response (responseBody),
    parseRequest,
  )
import Network.HTTP.Simple (httpLBS)
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Parser (noArguments)
import Tablebot.Plugin.Types
  ( Command (Command),
    Plugin (commands),
    plug,
  )

data CatAPI = CatAPI
  { breeds :: ![Text],
    id :: !Text,
    url :: !Text,
    width :: !Int,
    height :: !Int
  }
  deriving (Show)

instance FromJSON CatAPI where
  parseJSON (Object v) =
    CatAPI <$> v .: "breeds"
      <*> v .: "id"
      <*> v .: "url"
      <*> v .: "width"
      <*> v .: "height"
  parseJSON (Array v)
    | length v == 1 = parseJSON (v ! 0)
    | otherwise = mzero
  parseJSON _ = mzero

-- | @cat@ is a command that takes no arguments (using 'noArguments') and
-- replies with an image of a cat.
cat :: Command
cat =
  Command
    "cat"
    ( noArguments $ \m -> do
        r <- liftIO getCat
        _ <- sendMessage m r
        return ()
    )

getCatAPI :: IO (Either String CatAPI)
getCatAPI = do
  req <- parseRequest "https://api.thecatapi.com/v1/images/search"
  res <- httpLBS req
  return $ eitherDecode $ responseBody res

getCat :: IO Text
getCat = do
  response <- getCatAPI
  let catURL = case response of
        (Left _) -> "no cat today, sorry :("
        (Right r) -> url r
  return catURL

-- | @catPlugin@ assembles these commands into a plugin containing cat
catPlugin :: Plugin
catPlugin = plug {commands = [cat]}