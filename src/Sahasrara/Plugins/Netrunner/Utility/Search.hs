-- |
-- Module      : Sahasrara.Plugins.Netrunner.Utility.Search
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Converts plaintext to Netrunner search queries and vice versa.
module Sahasrara.Plugins.Netrunner.Utility.Search where

import Control.Monad.IO.Class (liftIO)
import Network.URI.Encode
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi)
import Sahasrara.Plugins.Netrunner.Type.Printing (Printing)
import Sahasrara.Utility
import Sahasrara.Utility.Json (Content, pageRequest)
import System.Environment (getEnv)

runSearch :: Int -> Int -> String -> EnvDatabaseDiscord NrApi (Content Printing)
runSearch pageSize pageIndex query = liftIO $ pageRequest pageSize (pageIndex * pageSize) "" "printings" ["filter%5Bdistinct_cards%5D", queryToFlag query]

queryToFlag :: String -> String
queryToFlag query = "filter%5Bsearch%5D=" ++ (encode query)

queryToLink :: String -> EnvDatabaseDiscord NrApi String
queryToLink query = do
  urlApi <- liftIO $ getEnv "API_URL"
  return $ urlApi ++ "printings?" ++ (queryToFlag query)

queryToNrdb :: String -> String
queryToNrdb query = "https://netrunnerdb.com/find/?q=" <> (encode query)
