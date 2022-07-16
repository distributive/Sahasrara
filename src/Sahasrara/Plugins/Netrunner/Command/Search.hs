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
import Data.Char (isSpace, toLower)
import Data.Either (isLeft, lefts, rights)
import Data.List (nub)
import Data.Map (lookup)
import Data.Text (pack, unpack)
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Print
import Sahasrara.Plugins.Netrunner.Utility.Search
import Sahasrara.Utility
import Sahasrara.Utility.Exception (BotException (GenericException, ParserException), throwBot)
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
        Nothing -> throwBot $ ParserException "No criteria provided" "Please specify some parameters\nUse the `help search` command for syntax"
        Just [] -> throwBot $ GenericException "No results" $ "No cards found for `" ++ unpack (pairsToNrdb pairs) ++ "`"
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
        Just [] -> throwBot $ GenericException "No results" $ "No cards found for `" ++ unpack (pairsToNrdb pairs) ++ "`"
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
    let decoded = map (fromShorthand api) ps
        pairs = fixSearch api $ rights decoded
        cards = searchCards api pairs
    if any isLeft decoded
      then throwBot $ ParserException "Shorthand not found" $ errorMsg $ lefts decoded
      else action cards pairs m
  where
    seps = [':', '!', '<', '>']
    ors = ['|']
    pair :: Parser (Either a (String, Char, [String]))
    pair = do
      cat <- underscoreWord
      sep <- satisfy (`elem` seps)
      content <- (quotedWithout ors <|> nonSpaceWord') `sepBy` satisfy (`elem` ors)
      return $ Right (cat, sep, content)
    nonSpaceWord' :: Parser String
    nonSpaceWord' = some $ satisfy $ \c -> not (isSpace c) && (c `notElem` ors)
    shorthand :: Parser (Either String b)
    shorthand = do
      w <- nonSpaceWord
      return $ Left w
    fromShorthand :: NrApi -> Either String (String, Char, [String]) -> Either String (String, Char, [String])
    fromShorthand api (Left s) = case lookup (map toLower s) $ shorthands api of
      Nothing -> Left s
      Just p -> Right p
    fromShorthand _ p = p
    errorMsg :: [String] -> String
    errorMsg ss = "No alias found for " ++ formatListOr (map (\s -> "`" ++ s ++ "`") $ nub ss) ++ "."
