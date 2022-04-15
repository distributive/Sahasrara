-- |
-- Module      : Sahasrara.Plugins.Netrunner.Command.Search
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for commands that search the card database.
module Sahasrara.Plugins.Netrunner.Command.Search (queryParser) where

import Control.Monad.Trans.Reader (ask)
import Data.Char (isSpace)
import Data.Map (lookup)
import Discord.Internal.Rest (Message)
import Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Search
import Sahasrara.Utility (EnvDatabaseDiscord, Parser)
import Sahasrara.Utility.Parser
import Text.Megaparsec
import Prelude hiding (lookup, unwords)

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
