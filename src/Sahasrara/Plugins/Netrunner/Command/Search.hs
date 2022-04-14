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
import Data.Map (Map, fromList, lookup)
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

-- | @shorthand@ maps plaintext shortcuts to explicit queries.
shorthands :: Map String (String, Char, [String])
shorthands =
  fromList
    [ ("corp", ("d", ':', ["corp"])),
      ("runner", ("d", ':', ["runner"])),
      ("identity", ("t", ':', ["identity"])),
      ("agenda", ("t", ':', ["agenda"])),
      ("asset", ("t", ':', ["asset"])),
      ("ice", ("t", ':', ["ice"])),
      ("operation", ("t", ':', ["operation"])),
      ("upgrade", ("t", ':', ["upgrade"])),
      ("event", ("t", ':', ["event"])),
      ("hardware", ("t", ':', ["hardware"])),
      ("program", ("t", ':', ["program"])),
      ("resource", ("t", ':', ["resource"])),
      ("hb", ("f", ':', ["haas-bioroid"])),
      ("haas-bioroid", ("f", ':', ["haas-bioroid"])),
      ("jinteki", ("f", ':', ["jinteki"])),
      ("nbn", ("f", ':', ["nbn"])),
      ("weyland", ("f", ':', ["weyland-consortium"])),
      ("weyland-consortium", ("f", ':', ["weyland-consortium"])),
      ("anarch", ("f", ':', ["anarch"])),
      ("criminal", ("f", ':', ["criminal"])),
      ("shaper", ("f", ':', ["shaper"])),
      ("adam", ("f", ':', ["adam"])),
      ("apex", ("f", ':', ["apex"])),
      ("sunny", ("f", ':', ["sunny-lebeau"])),
      ("sunny-lebeau", ("f", ':', ["sunny-lebeau"])),
      ("mini", ("f", ':', ["mini"])),
      ("neutral", ("f", ':', ["neutral-corp", "neutral-runner"])),
      ("free", ("o", ':', ["0"])),
      ("premium", ("o", '>', ["0"])),
      ("cheap", ("o", '<', ["3"])),
      ("expensive", ("o", '>', ["6"])),
      ("ambush", ("s", ':', ["ambush"])),
      ("bioroid", ("s", ':', ["bioroid"])),
      ("companion", ("s", ':', ["companion"])),
      ("friend", ("s", ':', ["companion"])),
      ("connection", ("s", ':', ["connection"])),
      ("console", ("s", ':', ["console"])),
      ("transaction", ("s", ':', ["transaction"])),
      ("icebreaker", ("s", ':', ["icebreaker"])),
      ("fracter", ("s", ':', ["fracter"])),
      ("decoder", ("s", ':', ["decoder"])),
      ("sentry", ("s", ':', ["sentry"])),
      ("ai", ("s", ':', ["ai"])),
      ("barrier", ("s", ':', ["barrier"])),
      ("code-gate", ("s", ':', ["code gate"])),
      ("sentry", ("s", ':', ["sentry"])),
      ("mythic", ("s", ':', ["mythic"])),
      ("grail", ("s", ':', ["grail"]))
    ]
