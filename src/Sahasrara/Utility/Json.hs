-- |
-- Module      : Sahasrara.Utility.Json
-- Description : Defines the Sahasrara colours in one place.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions for pinging JSON APIs.
module Sahasrara.Utility.Json where

import Data.Aeson (FromJSON, Value (Object), eitherDecode, parseJSON, (.:), (.:?))
import Data.Text (Text, unpack)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)

-- | @contentRequest@ makes an api call and wraps the result in a Content.
contentRequest :: FromJSON a => String -> String -> IO [a]
contentRequest label url = case label of
  "" -> contentRequest' url
  _ -> do
    cs <- contentRequest' url
    putStrLn $ label ++ ": " ++ (show $ length cs)
    return cs
  where
    contentRequest' :: FromJSON a => String -> IO [a]
    contentRequest' url' = do
      req <- parseRequest url'
      res <- httpLBS req
      case eitherDecode $ responseBody res of
        Left err -> putStrLn (url' <> " " <> err) >> (return $ content defaultContent)
        Right cData ->
          case link cData of
            Just next -> do
              dataNext <- contentRequest' $ unpack next
              return $ (content cData) ++ dataNext
            Nothing -> return $ content cData

-- | @Content@ represents raw data from the api.
data FromJSON a => Content a = Content
  { content :: ![a],
    link :: !(Maybe Text)
  }
  deriving (Show)

defaultContent :: FromJSON a => Content a
defaultContent = Content [] Nothing

instance FromJSON a => FromJSON (Content a) where
  parseJSON (Object v) = do
    content <- v .: "data"
    links <- v .:? "links"
    link <- do
      case links of
        Just ls -> ls .:? "next"
        Nothing -> return Nothing
    return $ Content content link
  parseJSON _ = return defaultContent
