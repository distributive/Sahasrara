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
import Control.Monad.Trans.Reader (ask)
import Data.List (intercalate, nub)
import Discord.Types
import Network.URI.Encode
import Sahasrara.Plugins.Netrunner.Type.Card (Card)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi)
import Sahasrara.Plugins.Netrunner.Utility.Printing (toCard)
import Sahasrara.Utility
import Sahasrara.Utility.Exception (BotException (EnvException), throwBot)
import Sahasrara.Utility.Json (contentRequest)
import Sahasrara.Utility.Parser
import System.Environment (lookupEnv)
import Text.Megaparsec

searchQueryParser :: Parser (Maybe String)
searchQueryParser = do
  queries <- many $ property <|> title
  return $ if queries == [] then Nothing else Just $ intercalate " " queries
  where
    property :: Parser String
    property = do
      (field, op, values) <- keyValuesSepOn ":!<>" "|"
      return $ field ++ [op] ++ (concat values)
    title :: Parser String
    title = do
      str <- quoted <|> nonSpaceWord
      return $ "_:\"" ++ str ++ "\""

applyToSearch :: ([Card] -> Message -> EnvDatabaseDiscord NrApi ()) -> String -> Message -> EnvDatabaseDiscord NrApi ()
applyToSearch func query m = do
  apiUrlM <- liftIO $ lookupEnv "API_URL"
  case apiUrlM of
    Nothing -> throwBot $ EnvException "API_URL not found in environment variables"
    Just apiUrl -> do
      api <- ask
      results <- liftIO $ contentRequest "" $ apiUrl ++ "printings?filter%5Bsearch%5D=" ++ (encode query)
      func (nub $ map (toCard api) results) m
