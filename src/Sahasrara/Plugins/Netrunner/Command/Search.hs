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
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Print
import Sahasrara.Plugins.Netrunner.Utility.Printing (toCard)
import Sahasrara.Plugins.Netrunner.Utility.Search
import Sahasrara.Utility
import Sahasrara.Utility.Exception (BotException (GenericException), throwBot)
import Sahasrara.Utility.Json (Content (Content))
import Sahasrara.Utility.Random (chooseOne, randomRange)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))
import Sahasrara.Utility.Types ()

-- | @nrSearch@ searches the card database with specific queries.
nrSearch :: EnvCommand NrApi
nrSearch = Command "search" (parseComm searchComm) []
  where
    searchComm :: Maybe Int -> RestOfInput String -> Message -> EnvDatabaseDiscord NrApi ()
    searchComm _ (ROI "") _ = throwBot $ GenericException "No criteria provided" "Please specify some parameters\nUse the `help search` command for syntax"
    searchComm Nothing q m = searchComm (Just 0) q m
    searchComm (Just page) (ROI query) m = do
      api <- ask
      Content results _ total <- runSearch 10 page query
      link <- queryToLink query
      case map (toCard api) results of
        [] -> throwBot $ GenericException "No results" $ "No cards found for `" ++ query ++ "`"
        [card] -> embedCard card m
        cs ->
          embedCards
            (":mag_right: **" <> pack (show total) <> " results**")
            ("Query: `" <> (pack query) <> "`\n")
            cs
            ("[...view in API](" <> pack link <> ")\n[...view on NRDB](" <> (pack $ queryToNrdb query) <> ") (new syntax unsupported)")
            ("[...view all in the API](" <> pack link <> ")\n[...view on NRDB](" <> (pack $ queryToNrdb query) <> ") (new syntax unsupported)")
            m

-- | @nrRandom@ searches the card database with specific queries and outputs a
-- single result at random.
nrRandom :: EnvCommand NrApi
nrRandom = Command "random" (parseComm randomComm) []
  where
    randomComm :: RestOfInput String -> Message -> EnvDatabaseDiscord NrApi ()
    randomComm (ROI "") m = do
      api <- ask
      card <- liftIO $ chooseOne $ cards api
      embedCard card m
    randomComm (ROI query) m = do
      api <- ask
      Content _ _ count <- runSearch 1 0 query -- Make a dummy request to grab the total number of results
      if count == 0
        then throwBot $ GenericException "No results" $ "No cards found for `" ++ query ++ "`"
        else return ()
      randomIndex <- liftIO $ randomRange 0 count
      Content results _ _ <- runSearch 1 randomIndex query -- Make the real request with a randomly generated index
      card <- liftIO $ chooseOne results
      embedCard (toCard api card) m
