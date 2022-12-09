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

import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, strip, toLower, unpack)
import Discord.Types
import Safe
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.Card (Card (title, factionCode))
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle)
import qualified Sahasrara.Plugins.Netrunner.Type.CardCycle as CardCycle
import Sahasrara.Plugins.Netrunner.Type.CardSet (CardSet)
import qualified Sahasrara.Plugins.Netrunner.Type.CardSet as CardSet
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Printing (Printing (cardSetCode))
import Sahasrara.Plugins.Netrunner.Utility.Card (toPrintings)
import Sahasrara.Plugins.Netrunner.Utility.Find
import Sahasrara.Plugins.Netrunner.Utility.Print (embedPrinting, embedPrintingFlavour, embedPrintingImg, embedRestrictionHistory)
import Sahasrara.Plugins.Netrunner.Utility.Printing (toCard, toCycle)
import Sahasrara.Utility
import Sahasrara.Utility.Random (chooseOne)
import Sahasrara.Utility.Discord (sendEmbedMessage)
import Sahasrara.Utility.Exception (BotException (GenericException), embedError)
import Sahasrara.Utility.Parser (inlineCommandHelper, integer, skipSpace)
import Sahasrara.Utility.Search (FuzzyCosts (..), closestValueWithCosts)
import Sahasrara.Utility.Types ()
import Text.Megaparsec (anySingleBut, satisfy, single, some, try, (<|>))

-- | @nrInline@ searches for cards by name.
nrInline :: EnvInlineCommand NrApi
nrInline = inlineCommandHelper "[[" "]]" (cardParser ']') $ outputCard embedPrinting

-- | @nrInlineImg@ searches for a card and outputs an image of it.
nrInlineImg :: EnvInlineCommand NrApi
nrInlineImg = inlineCommandHelper "{{" "}}" (cardParser '}') $ outputCard embedPrintingImg

-- | @nrInlineFlavour@ searches for a card and outputs its flavour.
nrInlineFlavour :: EnvInlineCommand NrApi
nrInlineFlavour = inlineCommandHelper "<<" ">>" (cardParser '>') $ outputCard embedPrintingFlavour

-- | @nrInlineBanHistory@ searches for a card and outputs its legality history.
nrInlineBanHistory :: EnvInlineCommand NrApi
nrInlineBanHistory = inlineCommandHelper "((" "))" (cardParser ')') $ outputCard embedBanHistory
  where
    embedBanHistory :: Printing -> Message -> EnvDatabaseDiscord NrApi ()
    embedBanHistory printing m = do
      api <- ask
      let card = toCard api printing
      embedRestrictionHistory card m

-- | @cardParser@ parses a card query and an optional specified set.
cardParser :: Char -> Parser (Text, Either Int Text)
cardParser c = try withSetIndex <|> try withSet <|> withoutSet
  where
    withSetIndex :: Parser (Text, Either Int Text)
    withSetIndex = do
      card <- some $ satisfy (`notElem` [c, '|'])
      _ <- single '|'
      skipSpace
      index <- integer
      skipSpace
      return (pack card, Left index)
    withSet :: Parser (Text, Either Int Text)
    withSet = do
      card <- some $ satisfy (`notElem` [c, '|'])
      _ <- single '|'
      set <- some $ satisfy (`notElem` [c, '|'])
      return (pack card, Right $ strip $ pack set)
    withoutSet :: Parser (Text, Either Int Text)
    withoutSet = do
      card <- some $ anySingleBut c
      return (pack card, Left (-1))

-- | @outputCard@ takes a function that displays a printing in some form (e.g.
-- by displaying its text or art) and generates a function that applies the
-- display function to a given search query and outputs the result or errors if
-- the query is invalid.
-- Errors are embedded manually, as errors thrown in inline commands are hidden.
outputCard :: (Printing -> Message -> EnvDatabaseDiscord NrApi ()) -> ((Text, Either Int Text) -> Message -> EnvDatabaseDiscord NrApi ())
outputCard outf = \(query, set) m -> do
  api <- ask
  card <-
    case toLower query of
      "me" -> pure $ queryCard api $ messageAuthorName m
      ":|" -> liftIO $ chooseOne $ filter ((`elem` ["neutral_corp", "neutral_runner"]) . factionCode) $ cards api
      _ -> pure $ queryCard api query
  let printings = reverse $ toPrintings api card
  case set of
    Left (-1) -> outf (headNote (show card) printings) m
    Left index ->
      let i = if index < 0 then length printings + index else index
       in if i < 0 || i >= length printings
            then sendEmbedMessage m "" $ errorIndex index $ title card
            else outf (printings !! i) m
    Right set' ->
      let mSet = matchSet api set'
       in case find (setFilter api mSet) printings of
            Just card' -> outf card' m
            Nothing -> case mSet of
              Left p -> sendEmbedMessage m "" $ errorNotFound (CardSet.name p) $ title card
              Right c -> sendEmbedMessage m "" $ errorNotFound (CardCycle.name c) $ title card
  where
    setFilter :: NrApi -> Either CardSet CardCycle -> (Printing -> Bool)
    setFilter _ (Left cs) = \printing -> cardSetCode printing == CardSet.code cs
    setFilter api (Right cc) = \printing -> cc == toCycle api printing
    matchSet :: NrApi -> Text -> Either CardSet CardCycle
    matchSet api set =
      case (matchedSet, matchedCycle, closestSet) of
        (Just p, _, _) -> Left p
        (_, Just c, _) -> Right c
        (_, _, fuzzyMatch) -> fuzzyMatch
      where
        matchedSet :: Maybe CardSet
        matchedSet = find (\p -> standardise (CardSet.code p) == standardise set) $ cardSets api
        matchedCycle :: Maybe CardCycle
        matchedCycle = find (\c -> standardise (CardCycle.code c) == standardise set) $ cardCycles api
        closestSet :: Either CardSet CardCycle
        closestSet =
          let ls = zip (unpack <$> CardSet.name <$> cardSets api) (Left <$> cardSets api)
              rs = zip (unpack <$> CardCycle.name <$> cardCycles api) (Right <$> cardCycles api)
           in closestValueWithCosts editCosts (ls ++ rs) $ unpack $ standardise set
        editCosts :: FuzzyCosts
        editCosts =
          FuzzyCosts
            { deletion = 1,
              insertion = 0,
              substitution = 1,
              transposition = 1
            }
    messageAuthorName :: Message -> Text
    messageAuthorName m =
      let uName = userName $ messageAuthor m
       in case messageMember m of
            Nothing -> uName
            Just guildMember -> fromMaybe uName $ memberNick guildMember
    errorNotFound :: Text -> Text -> CreateEmbed
    errorNotFound set card = embedError $ GenericException "Set does not contain card" $ "`" <> (unpack set) <> "` does not contain *" <> unpack card <> "*."
    errorIndex :: Int -> Text -> CreateEmbed
    errorIndex index card = embedError $ GenericException "Invalid index" $ "`" <> show index <> "` is out of range.\nTry `sets " <> unpack card <> "` to see how many sets it was printed in."
