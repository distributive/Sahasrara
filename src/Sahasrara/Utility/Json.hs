-- |
-- Module      : Sahasrara.Utility.Json
-- Description : Defines the Sahasrara colours in one place.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions for pinging JSON APIs.
module Sahasrara.Utility.Json (
  Content (..),
  contentRequest,
  pageRequest
  ) where

import Data.Aeson (FromJSON, Value (Object), eitherDecode, parseJSON, (.:), (.:?))
import Data.Char (isDigit)
import Data.Text (Text, unpack)
import Data.List (intercalate)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)
import System.Environment (getEnv)

-- | @fetch@ makes an api call and wraps the result in a Content.
-- If paginate is true, continue fetching data until all pages are parsed
fetch :: FromJSON a => Bool -> String -> String -> [String] -> IO (Content a)
fetch paginate label url flags = do
  apiUrl <- getEnv "API_URL"
  let url' = apiUrl ++ url ++ "?" ++ (intercalate "&" flags)
  case label of
    "" -> fetch' url'
    _ -> do
      cs <- fetch' url'
      putStrLn $ label ++ ": " ++ (show $ length $ content cs)
      return cs
  where
    fetch' :: FromJSON a => String -> IO (Content a)
    fetch' url' = do
      req <- parseRequest url'
      res <- httpLBS req
      case eitherDecode $ responseBody res of
        Left err -> putStrLn (url' <> " " <> err) >> (return defaultContent)
        Right cData ->
          if paginate
            then
              case next cData of
                Just n -> do
                  dataNext <- fetch' $ unpack n
                  return $ Content ((content cData) ++ (content dataNext)) (next cData) (count cData)
                Nothing -> return cData
            else
              return cData

-- | @contentRequest@ gets all content from a link, including additional pages.
contentRequest :: FromJSON a => String -> String -> [String] -> IO (Content a)
contentRequest = fetch True

-- | @pageRequest@ gets exactly one page of data.
pageRequest :: FromJSON a => Int -> Int -> String -> String -> [String] -> IO (Content a)
pageRequest pageSize pageIndex label url flags =
  let pageFlags = ["page%5Blimit%5D=" ++ show pageSize, "page%5Boffset%5D=" ++ show pageIndex]
   in fetch False label url $ pageFlags ++ flags

-- | @Content@ represents raw data from the api.
-- Ormolu doesn't understand typeclass constraints in this context so it's disabled
{- ORMOLU_DISABLE -}
data FromJSON a => Content a = Content
  { content :: ![a],
    next :: !(Maybe Text),
    count :: !Int
  }
  deriving (Show)
{- ORMOLU_ENABLE -}

defaultContent :: FromJSON a => Content a
defaultContent = Content [] Nothing 0

instance FromJSON a => FromJSON (Content a) where
  parseJSON (Object v) = do
    content <- v .: "data"
    links <- v .:? "links"
    next <- do
      case links of
        Just ls -> ls .:? "next"
        Nothing -> return Nothing
    count <- do
      case links of
        Nothing -> return $ length content
        Just ls -> do
          lst <- ls .: "last"
          return $ (length content) + (read $ reverse $ takeWhile isDigit $ reverse lst)
    return $ Content content next count
  parseJSON _ = return defaultContent
