-- |
-- Module      : Sahasrara.Plugins.Netrunner.Command.Find
-- Description : A command that flips a coin, or randomly selects from a list.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for getting Netrunner cards.
module Sahasrara.Plugins.Netrunner.Command.Find (nrInline, nrInlineImg, nrInlineFlavour, nrInlineBanHistory) where

import Control.Monad.Trans.Reader (ask)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, strip, unpack)
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.Card (Card (packCode, title))
import Sahasrara.Plugins.Netrunner.Type.Cycle (Cycle)
import qualified Sahasrara.Plugins.Netrunner.Type.Cycle as C
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Pack (Pack)
import qualified Sahasrara.Plugins.Netrunner.Type.Pack as P
import Sahasrara.Plugins.Netrunner.Utility.Find
import Sahasrara.Plugins.Netrunner.Utility.Print (embedBanHistory, embedCard, embedCardFlavour, embedCardImg)
import Sahasrara.Utility
import Sahasrara.Utility.Discord (sendEmbedMessage)
import Sahasrara.Utility.Exception (BotException (GenericException), embedError)
import Sahasrara.Utility.Parser (inlineCommandHelper, integer, skipSpace)
import Sahasrara.Utility.Search (FuzzyCosts (..), closestValueWithCosts)
import Sahasrara.Utility.Types ()
import Text.Megaparsec (anySingleBut, single, some, try, (<|>))

-- | @nrInline@ searches for cards by name.
nrInline :: EnvInlineCommand NrApi
nrInline = inlineCommandHelper "[[" "]]" (cardParser ']') $ outputCard embedCard

-- | @nrInlineImg@ searches for a card and outputs an image of it.
nrInlineImg :: EnvInlineCommand NrApi
nrInlineImg = inlineCommandHelper "{{" "}}" (cardParser '}') $ outputCard embedCardImg

-- | @nrInlineFlavour@ searches for a card and outputs its flavour.
nrInlineFlavour :: EnvInlineCommand NrApi
nrInlineFlavour = inlineCommandHelper "<<" ">>" (cardParser '>') $ outputCard embedCardFlavour

-- | @nrInlineBanHistory@ searches for a card and outputs its legality history.
nrInlineBanHistory :: EnvInlineCommand NrApi
nrInlineBanHistory = inlineCommandHelper "((" "))" (cardParser ')') $ outputCard embedBanHistory

-- | @cardParser@ parses a card and an optional specified set.
cardParser :: Char -> Parser (Text, Either Int Text)
cardParser c = try withSetIndex <|> try withSet <|> withoutSet
  where
    withSetIndex :: Parser (Text, Either Int Text)
    withSetIndex = do
      card <- some $ anySingleBut '|'
      _ <- single '|'
      skipSpace
      index <- integer
      skipSpace
      return (pack card, Left index)
    withSet :: Parser (Text, Either Int Text)
    withSet = do
      card <- some $ anySingleBut '|'
      _ <- single '|'
      set <- some $ anySingleBut c
      return (pack card, Right $ strip $ pack set)
    withoutSet :: Parser (Text, Either Int Text)
    withoutSet = do
      card <- some $ anySingleBut c
      return (pack card, Left (-1))

-- | @outputCard@ takes a function that displays a card in some form (e.g. by
-- displaying its text or art) and generates a function that applies the display
-- function to a given search query and outputs the result or errors if the
-- query is invalid.
-- Errors are embedded manually as errors thrown in inline commands are hidden.
outputCard :: (Card -> Message -> EnvDatabaseDiscord NrApi ()) -> ((Text, Either Int Text) -> Message -> EnvDatabaseDiscord NrApi ())
outputCard outf = \(card, set) m -> do
  api <- ask
  let printings = reverse $ queryPrintings api card
  case set of
    Left (-1) -> outf (queryCard api card) m
    Left index ->
      let i = if index < 0 then length printings + index else index
       in if i < 0 || i >= length printings
            then sendEmbedMessage m "" $ errorIndex index $ fromMaybe "?" $ title $ head printings
            else outf (printings !! i) m
    Right set' ->
      let mSet = matchedSet api set'
       in case find (setFilter mSet) printings of
            Just card' -> outf card' m
            Nothing -> case mSet of
              Left p -> sendEmbedMessage m "" $ errorNotFound (P.name p) $ fromMaybe "?" $ title $ head printings
              Right c -> sendEmbedMessage m "" $ errorNotFound (C.name c) $ fromMaybe "?" $ title $ head printings
  where
    setFilter :: Either Pack Cycle -> (Card -> Bool)
    setFilter (Left p) = (\card -> packCode card == (Just $ P.code p))
    setFilter (Right c) = (\card -> packCode card == (Just $ C.code c))
    matchedSet :: NrApi -> Text -> Either Pack Cycle
    matchedSet api set =
      case (matchedPack, matchedCycle, closestSet) of
        (Just p, _, _) -> Left p
        (_, Just c, _) -> Right c
        (_, _, fuzzyMatch) -> fuzzyMatch
      where
        matchedPack :: Maybe Pack
        matchedPack = find (\p -> standardise (P.code p) == standardise set) $ packs api
        matchedCycle :: Maybe Cycle
        matchedCycle = find (\c -> standardise (C.code c) == standardise set) $ cycles api
        closestSet :: Either Pack Cycle
        closestSet =
          let ls = zip (unpack <$> P.name <$> packs api) (Left <$> packs api)
              rs = zip (unpack <$> C.name <$> cycles api) (Right <$> cycles api)
           in closestValueWithCosts editCosts (ls ++ rs) $ unpack $ standardise set
        editCosts :: FuzzyCosts
        editCosts =
          FuzzyCosts
            { deletion = 1,
              insertion = 0,
              substitution = 1,
              transposition = 1
            }
    errorNotFound :: Text -> Text -> Embed
    errorNotFound set card = embedError $ GenericException "Set does not contain card" $ "`" <> (unpack set) <> "` does not contain *" <> unpack card <> "*."
    errorIndex :: Int -> Text -> Embed
    errorIndex index card = embedError $ GenericException "Invalid index" $ "`" <> show index <> "` is out of range.\nTry `sets " <> unpack card <> "` to see how many sets it was printed in."
