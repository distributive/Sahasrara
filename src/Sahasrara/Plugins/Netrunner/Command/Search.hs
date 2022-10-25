-- |
-- Module      : Sahasrara.Plugins.Netrunner.Command.Search
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for commands that search the card database.
module Sahasrara.Plugins.Netrunner.Command.Search (nrSearch, nrRandom) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Text (pack)
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Print
import Sahasrara.Plugins.Netrunner.Utility.Search
import Sahasrara.Utility
import Sahasrara.Utility.Exception (BotException (GenericException), throwBot)
import Sahasrara.Utility.Random (chooseOne)
import Sahasrara.Utility.Types ()

-- | @nrSearch@ searches the card database with specific queries.
nrSearch :: EnvCommand NrApi
nrSearch = Command "search" searchComm []
  where
    searchComm :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    searchComm = do
      queryM <- searchQueryParser
      return $ case queryM of
        Nothing -> \_ -> throwBot $ GenericException "No criteria provided" "Please specify some parameters\nUse the `help search` command for syntax"
        Just query -> applyToSearch (listResultsOfQuery query) query
    listResultsOfQuery :: String -> [Card] -> Message -> EnvDatabaseDiscord NrApi ()
    listResultsOfQuery query results m =
      case results of
        [] -> throwBot $ GenericException "No results" $ "No cards found for `" ++ query ++ "`"
        [card] -> embedCard card m
        cs ->
          embedCards
            ("Query: `" <> (pack query) <> "`\n")
            cs
            ""
            "" -- TODO: Make these links to nrdb when it's updated to the new search algo
            m

-- | @nrRandom@ searches the card database with specific queries and outputs a
-- single result at random.
nrRandom :: EnvCommand NrApi
nrRandom = Command "random" randomPars []
  where
    randomPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    randomPars = do
      queryM <- searchQueryParser
      return $ case queryM of
        Nothing -> randomCard
        Just query -> applyToSearch (randomResultOfQuery query) query
    randomCard :: Message -> EnvDatabaseDiscord NrApi ()
    randomCard m = do
      api <- ask
      card <- liftIO $ chooseOne $ cards api
      embedCard card m
    randomResultOfQuery :: String -> [Card] -> Message -> EnvDatabaseDiscord NrApi ()
    randomResultOfQuery query results m = case results of
      [] -> throwBot $ GenericException "No results" $ "No cards found for `" ++ query ++ "`"
      _ -> do
        card <- liftIO $ chooseOne results
        embedCard card m
