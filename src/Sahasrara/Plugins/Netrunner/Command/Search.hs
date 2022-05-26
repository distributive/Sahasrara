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
import Data.Char (isSpace)
import Data.Map (lookup)
import Data.Text (pack)
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Print
import Sahasrara.Plugins.Netrunner.Utility.Search
import Sahasrara.Utility
import Sahasrara.Utility.Discord (sendMessage)
import Sahasrara.Utility.Parser
import Sahasrara.Utility.Random (chooseOne)
import Sahasrara.Utility.Types ()
import Text.Megaparsec
import Prelude hiding (lookup, unwords)

-- | @nrSearch@ searches the card database with specific queries.
nrSearch :: EnvCommand NrApi
nrSearch = Command "search" searchPars []
  where
    searchPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    searchPars = queryParser $ \cs pairs m -> do
      case cs of
        Nothing -> sendMessage m "No criteria provided!"
        Just [] -> sendMessage m $ "No cards found for `" <> pairsToNrdb pairs <> "`"
        Just [card] -> embedCard card m
        Just cards ->
          embedCards
            ("Query: `" <> pairsToNrdb pairs <> "`\n")
            cards
            ("_[...view on NRDB](" <> pairsToQuery pairs <> ")_")
            ("_[..." <> pack (show $ length cards - 10) <> " more](" <> pairsToQuery pairs <> ")_")
            m

-- | @nrRandom@ searches the card database with specific queries and outputs a
-- single result at random.
nrRandom :: EnvCommand NrApi
nrRandom = Command "random" randomPars []
  where
    randomPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    randomPars = queryParser $ \cs pairs m -> do
      case cs of
        Nothing -> do
          api <- ask
          card <- liftIO $ chooseOne $ cards api
          embedCard card m
        Just [] -> sendMessage m $ "No cards found for `" <> pairsToNrdb pairs <> "`"
        Just cards -> do
          card <- liftIO $ chooseOne cards
          embedCard card m

-- | @queryParser@ extracts search queries from plaintext.
queryParser ::
  (Maybe [Card] -> [Query] -> Message -> EnvDatabaseDiscord NrApi ()) ->
  Parser (Message -> EnvDatabaseDiscord NrApi ())
queryParser action = do
  ps <- many $ skipSpace *> (try pair <|> shorthand)
  return $ \m -> do
    api <- ask
    let pairs = fixSearch api ps
        cards = searchCards api pairs
    action cards pairs m
  where
    seps = [':', '!', '<', '>']
    ors = ['|']
    pair :: Parser (String, Char, [String])
    pair = do
      cat <- underscoreWord
      sep <- satisfy (`elem` seps)
      content <- (quotedWithout ors <|> nonSpaceWord') `sepBy` satisfy (`elem` ors)
      return (cat, sep, content)
    shorthand :: Parser (String, Char, [String])
    shorthand = do
      w <- nonSpaceWord
      case lookup w shorthands of
        Just p -> return p
        Nothing -> fail $ "No alias found for " ++ w
    nonSpaceWord' :: Parser String
    nonSpaceWord' = some $ satisfy $ \c -> not (isSpace c) && (c `notElem` ors)
