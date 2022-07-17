{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Print
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The backend functionality of the Netrunner commands.
module Sahasrara.Plugins.Netrunner.Utility.Print
  ( embedCard,
    embedCards,
    embedCardImg,
    embedCardFlavour,
    embedCardSets,
    embedSets,
    embedSetsOn,
    embedCycles,
    embedBanHistory,
    embedBanLists,
    embedBanList,
  )
where

import Control.Monad.Trans.Reader (ask)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, intercalate)
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.BanList (BanList (active), CardBan (..))
import qualified Sahasrara.Plugins.Netrunner.Type.BanList as BanList
import Sahasrara.Plugins.Netrunner.Type.Card (Card (code, text))
import Sahasrara.Plugins.Netrunner.Type.Cycle (Cycle)
import qualified Sahasrara.Plugins.Netrunner.Type.Cycle as C
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import qualified Sahasrara.Plugins.Netrunner.Type.Pack as P
import Sahasrara.Plugins.Netrunner.Utility.BanList (activeBanList, latestBanListActive, listAffectedCards, listBanHistory, listBanLists, toMwlStatus)
import Sahasrara.Plugins.Netrunner.Utility.Card (toPack)
import Sahasrara.Plugins.Netrunner.Utility.Embed
import Sahasrara.Plugins.Netrunner.Utility.Find
import Sahasrara.Plugins.Netrunner.Utility.Format (formatText)
import Sahasrara.Utility
import Sahasrara.Utility.Discord (formatFromEmojiName, sendEmbedMessage)
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Types ()

-- | @embedCard@ takes a card and embeds it in a message.
embedCard :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCard card m = do
  api <- ask
  sendEmbedMessage m "" =<< cardToEmbed api card

-- | @embedCards@ takes a list of cards and embeds their names.
embedCards :: Text -> [Card] -> Text -> Text -> Message -> EnvDatabaseDiscord NrApi ()
embedCards pre cards post err m = do
  api <- ask
  sendEmbedMessage m "" =<< cardsToEmbed api pre cards post err

-- | @embedCardImg@ embeds a card's image in a message, if able.
embedCardImg :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCardImg card m = do
  api <- ask
  let card' = case code card of
        Just "13026" -> queryCard api "Bio-Modeled Network"
        Just "12006" -> queryCard api "Biometric Spoofing"
        _ -> card
  sendEmbedMessage m "" $ cardToImgEmbed api card'

-- | @embedCardFlavour@ embeds a card's flavour in a message, if able.
embedCardFlavour :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCardFlavour card m = do
  api <- ask
  let card' = case code card of
        Just "07024" -> queryCard api "Déjà Vu"
        Just "01002" -> queryCard api "The Twins"
        _ -> card
  cText <- formatText $ fromMaybe "" $ text card'
  embed <- case code card' of
    Just "12077" -> cardToEmbedWithText api card' cText
    _ -> cardToFlavourEmbed api card'
  sendEmbedMessage m "" embed

-- | @embedCardSets@ embeds a list of packs a card was printed in.
embedCardSets :: Text -> Message -> EnvDatabaseDiscord NrApi ()
embedCardSets card m = do
  api <- ask
  let printings = queryPrintings api card
      sets = mapMaybe (toPack api) printings
      entries = map (\s -> "`" <> P.code s <> "` - " <> P.name s) sets
  embed <- cardToEmbedWithText api (head printings) $ intercalate "\n" entries
  sendEmbedMessage m "" embed

-- | @embedSets@ embeds all sets from Netrunner history.
embedSets :: Message -> EnvDatabaseDiscord NrApi ()
embedSets =
  let title = ":card_box: All Netrunner sets :card_box:"
      url = "https://netrunnerdb.com/en/sets"
      pre = ":white_check_mark: legal | :repeat: rotated | :no_entry_sign: never legal in standard"
   in embedSetsOn title url pre (\_ -> True)

-- | @embedSetsOn@ embeds all sets from Netrunner history that fulfil a predicate.
embedSetsOn :: Text -> Text -> Text -> (Cycle -> Bool) -> Message -> EnvDatabaseDiscord NrApi ()
embedSetsOn title url pre predicate m = do
  api <- ask
  sep <- formatFromEmojiName "s_subroutine"
  let cols = mapMaybe (formatCycle api $ sep <> " ") $ filter predicate $ cycles api
      ordered = filter isCycle cols ++ filter (not . isCycle) cols
  sendEmbedMessage m "" $ addColour DiscordColorBlue $ embedColumnsWithUrl title url pre ordered
  where
    formatCycle :: NrApi -> Text -> Cycle -> Maybe (Text, [Text])
    formatCycle NrApi {packs = packs} sep c =
      case filter (\p -> P.cycleCode p == C.code c) packs of
        [] -> Nothing
        ps -> Just (cycleName c, map ((sep <>) . P.name) ps)
    cycleName :: Cycle -> Text
    cycleName c =
      (if isSpecial c then ":no_entry_sign: " else if C.rotated c then ":repeat: " else ":white_check_mark: ")
        <> C.name c
    isSpecial :: Cycle -> Bool
    isSpecial c = case C.code c of
      "draft" -> True
      "napd" -> True
      _ -> False
    isCycle :: (Text, [Text]) -> Bool
    isCycle (_, xs) = length xs > 2

-- | @embedCycles@ embeds the name of each Netrunner cycle.
embedCycles :: Message -> EnvDatabaseDiscord NrApi ()
embedCycles m = do
  api <- ask
  let title = ":recycle: All Netrunner cycles :recycle:"
      url = "https://netrunnerdb.com/en/sets"
      pre = ":white_check_mark: legal | :repeat: rotated"
      list = intercalate "\n" $ map formatCycle $ filter (isCycle api) $ cycles api
      text = pre <> "\n\n" <> list
  sendEmbedMessage m "" $ addColour DiscordColorBlue $ embedTextWithUrl title url text
  where
    formatCycle :: Cycle -> Text
    formatCycle c = (if C.rotated c then ":repeat: " else ":white_check_mark: ") <> C.name c
    isCycle :: NrApi -> Cycle -> Bool
    isCycle NrApi {packs = packs} c =
      let count = length $ filter (\p -> P.cycleCode p == C.code c) packs
       in count > 2

-- | @embedBanHistory@ embeds a card's banlist history.
embedBanHistory :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedBanHistory card m = do
  api <- ask
  embed <- cardToEmbedWithText api card $ listBanHistory api card
  let colour = case toMwlStatus api (activeBanList api) card of
        Banned -> DiscordColorRed
        Legal -> DiscordColorGreen
        _ -> DiscordColorYellow
  sendEmbedMessage m "" $ addColour colour embed

-- | @embedBanLists@ embeds all banlists in Netrunner history.
embedBanLists :: Message -> EnvDatabaseDiscord NrApi ()
embedBanLists m = do
  api <- ask
  let embed = embedTextWithUrl "Standard Banlists" "https://netrunnerdb.com/en/banlists" $ listBanLists api
      colour = if latestBanListActive api then DiscordColorRed else DiscordColorYellow
  sendEmbedMessage m "" $ addColour colour embed

-- | @embedBanList@ embeds the list of cards affected by a given banlist.
embedBanList :: BanList -> Message -> EnvDatabaseDiscord NrApi ()
embedBanList banList m = do
  api <- ask
  let (pre, cCards, rCards) = listAffectedCards api banList
      header = BanList.name banList <> if active banList then " (active)" else ""
      colour = if active banList then DiscordColorRed else DiscordColorYellow
  sendEmbedMessage m "" $ addColour colour $ embedColumns header pre [("Corp Cards", cCards), ("Runner Cards", rCards)]
