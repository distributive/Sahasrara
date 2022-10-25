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
  ( embedPrinting,
    embedCard,
    embedPrintings,
    embedCards,
    embedPrintingImg,
    embedPrintingFlavour,
    embedCardSets,
    embedCardQuerySets,
    embedSets,
    embedSetsOn,
    embedCycle,
    embedCycles,
    embedRestrictionHistory,
    embedRestrictions,
    embedRestriction,
  )
where

import Control.Monad.Trans.Reader (ask)
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Data.Text (Text, intercalate)
import Discord.Types
import Safe
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.Card (Card (text))
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle)
import qualified Sahasrara.Plugins.Netrunner.Type.CardCycle as CardCycle
import Sahasrara.Plugins.Netrunner.Type.CardSet (CardSet)
import qualified Sahasrara.Plugins.Netrunner.Type.CardSet as CardSet
import Sahasrara.Plugins.Netrunner.Type.Format (Format)
import qualified Sahasrara.Plugins.Netrunner.Type.Format as Format
import Sahasrara.Plugins.Netrunner.Type.Legality (Legality (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (cardCycles, cardSets))
import Sahasrara.Plugins.Netrunner.Type.Printing (Printing (code))
import Sahasrara.Plugins.Netrunner.Type.Restriction (Restriction)
import qualified Sahasrara.Plugins.Netrunner.Type.Restriction as Restriction
import Sahasrara.Plugins.Netrunner.Utility.Card
import Sahasrara.Plugins.Netrunner.Utility.CardCycle
import Sahasrara.Plugins.Netrunner.Utility.CardSet (toCycleName)
import Sahasrara.Plugins.Netrunner.Utility.Embed
import Sahasrara.Plugins.Netrunner.Utility.Find
import Sahasrara.Plugins.Netrunner.Utility.Format
import Sahasrara.Plugins.Netrunner.Utility.Formatting (formatText)
import Sahasrara.Plugins.Netrunner.Utility.Legality
import Sahasrara.Plugins.Netrunner.Utility.Printing (toCard, toCardSet)
import Sahasrara.Plugins.Netrunner.Utility.Symbol
import Sahasrara.Utility
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (formatFromEmojiName, sendEmbedMessage)
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Types ()

-- | @embedPrinting@ takes a printing and embeds it in a message.
embedPrinting :: Printing -> Message -> EnvDatabaseDiscord NrApi ()
embedPrinting printing m = do
  api <- ask
  sendEmbedMessage m "" =<< printingToEmbed api printing

-- | @embedCard@ takes a card and embeds its latest printing in a message.
embedCard :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCard card m = do
  api <- ask
  embedPrinting (toLatestPrinting api card) m

-- | @embedPrintings@ takes a list of printings and embeds their names.
embedPrintings :: Text -> Text -> [Printing] -> Text -> Text -> Message -> EnvDatabaseDiscord NrApi ()
embedPrintings header pre printings post err m = do
  api <- ask
  sendEmbedMessage m "" =<< printingsToEmbed api header pre printings post err

-- | @embedCards@ takes a list of cards and embeds their names.
embedCards :: Text -> Text -> [Card] -> Text -> Text -> Message -> EnvDatabaseDiscord NrApi ()
embedCards header pre cards post err m = do
  api <- ask
  embedPrintings header pre (map (toLatestPrinting api) cards) post err m

-- | @embedPrintingImg@ embeds a card's image in a message, if able.
embedPrintingImg :: Printing -> Message -> EnvDatabaseDiscord NrApi ()
embedPrintingImg printing m = do
  api <- ask
  let printing' = case code printing of
        "13026" -> toLatestPrinting api $ queryCard api "Bio-Modeled Network"
        "12006" -> toLatestPrinting api $ queryCard api "Biometric Spoofing"
        _ -> printing
  sendEmbedMessage m "" $ printingToImgEmbed api printing'

-- | @embedPrintingFlavour@ embeds a card's flavour in a message, if able.
embedPrintingFlavour :: Printing -> Message -> EnvDatabaseDiscord NrApi ()
embedPrintingFlavour printing m = do
  api <- ask
  let printing' = case code printing of
        "07024" -> toLatestPrinting api $ queryCard api "Déjà Vu"
        "01002" -> toLatestPrinting api $ queryCard api "The Twins"
        _ -> printing
  cText <- formatText $ text $ toCard api $ printing'
  embed <- case code printing' of
    "12077" -> printingToEmbedWithText api printing' cText
    _ -> printingToFlavourEmbed api printing'
  sendEmbedMessage m "" embed

-- | @embedCardSets@ embeds a list of sets a card was printed in.
embedCardSets :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCardSets card m = do
  api <- ask
  let printings = toPrintings api card
      sets = map (toCardSet api) printings
      entries = map (\s -> "`" <> CardSet.code s <> "` - " <> CardSet.name s <> cycleName api s) sets
  embed <- printingToEmbedWithText api (headNote "6" printings) $ intercalate "\n" entries
  sendEmbedMessage m "" embed
  where
    cycleName :: NrApi -> CardSet -> Text
    cycleName api set = case toCycleName api set of
      Nothing -> ""
      Just name ->
        if name == CardSet.name set
          then ""
          else " (" <> name <> ")"

-- | @embedCardQuerySets@ queries a card name and embeds a list of sets it was printed in.
embedCardQuerySets :: Text -> Message -> EnvDatabaseDiscord NrApi ()
embedCardQuerySets query m = do
  api <- ask
  embedCardSets (queryCard api query) m

-- | @embedSets@ embeds all sets from Netrunner history.
embedSets :: Message -> EnvDatabaseDiscord NrApi ()
embedSets =
  let title = ":card_box: All Netrunner sets :card_box:"
      url = "https://netrunnerdb.com/en/sets"
      pre = symbolLegal <> " legal | " <> symbolRotated <> " rotated | " <> symbolInvalid <> " never legal in Standard"
   in embedSetsOn title url pre (\_ -> True)

-- | @embedSetsOn@ embeds all card sets from that fulfil a predicate.
embedSetsOn :: Text -> Text -> Text -> (CardCycle -> Bool) -> Message -> EnvDatabaseDiscord NrApi ()
embedSetsOn title url pre predicate m = do
  api <- ask
  sep <- formatFromEmojiName "s_subroutine"
  let (cycles, others) = partition (isCycle api) $ reverse $ cardCycles api
      (nsg, ffg) = partition (isNsg api) cycles
      (bigBoxes, miscs) = partition (isBigBox api) others
      cols =
        (formatCols api sep $ nsg)
          ++ generateFiller (length nsg)
          ++ (formatCols api sep $ ffg)
          ++ generateFiller (length ffg)
          ++ [ flattenCols "Big boxes" $ formatCols api sep bigBoxes,
               flattenCols "Miscellanious " $ formatCols api sep miscs
             ]
  sendEmbedMessage m "" $ addColour colInfo $ embedColumnsWithUrl title url (formatPre cycles) cols
  where
    formatCols :: NrApi -> Text -> [CardCycle] -> [(Text, [Text])]
    formatCols api sep cs = mapMaybe (formatCycle api $ sep <> " ") $ filter predicate cs
    formatCycle :: NrApi -> Text -> CardCycle -> Maybe (Text, [Text])
    formatCycle api sep c =
      case filter (\p -> CardSet.cycleCode p == CardCycle.code c) $ cardSets api of
        [] -> Nothing
        [_] -> Just (formatCycleNameLegality api c, [])
        ps -> Just (formatCycleNameLegality api c, map ((sep <>) . CardSet.name) ps)
    generateFiller :: Int -> [(Text, [Text])]
    generateFiller 0 = []
    generateFiller count = replicate (3 - count `mod` 3) emptyField
    flattenCols :: Text -> [(Text, [Text])] -> (Text, [Text])
    flattenCols header cols = (header, concatMap (\(hd, sets) -> hd : sets) cols)
    formatPre :: [CardCycle] -> Text
    formatPre [] = pre
    formatPre _ = pre <> "\n\n**Cycles**"

-- | @embedCycle@ embeds all sets from a single cycle.
embedCycle :: Text -> Text -> Text -> CardCycle -> Message -> EnvDatabaseDiscord NrApi ()
embedCycle title url pre c m = do
  api <- ask
  sep <- formatFromEmojiName "s_subroutine"
  let header = formatCycleNameLegality api c
      sets = intercalate "\n" $ map (((sep <> " ") <>) . CardSet.name) $ filter (\p -> CardSet.cycleCode p == CardCycle.code c) $ cardSets api
  sendEmbedMessage m "" $ addColour colInfo $ embedTextWithUrl title url $ pre <> "\n\n" <> header <> "\n" <> sets

-- | @embedCycles@ embeds the name of each Netrunner cycle.
embedCycles :: Message -> EnvDatabaseDiscord NrApi ()
embedCycles m = do
  api <- ask
  let title = ":recycle: All Netrunner cycles :recycle:"
      url = "https://netrunnerdb.com/en/sets"
      pre = symbolLegal <> " legal | " <> symbolRotated <> " rotated"
      list = intercalate "\n" $ map (formatCycleNameLegality api) $ filter (isCycle api) $ reverse $ cardCycles api
      text = pre <> "\n\n" <> list
  sendEmbedMessage m "" $ addColour colInfo $ embedTextWithUrl title url text

-- | @embedRestrictionHistory@ embeds a card's restriction history.
embedRestrictionHistory :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedRestrictionHistory card m = do
  api <- ask
  embed <- printingToEmbedWithText api (toLatestPrinting api card) $ listHistory api (getStandard api) card
  let colour = case toLegality api (toActiveSnapshot api $ getStandard api) card of
        Legal -> colLegal
        Rotated -> colRotated
        Invalid -> colInvalid
        Banned -> colBanned
        Restricted -> colRestricted
        UniversalFactionCost _ -> colUniversalFactionCost
        GlobalPenalty -> colGlobalPenalty
        Points _ -> colPoints
  sendEmbedMessage m "" $ addColour colour embed

-- | @embedRestrictions@ embeds all restrictions in Netrunner history.
embedRestrictions :: Format -> Message -> EnvDatabaseDiscord NrApi ()
embedRestrictions format m = do
  api <- ask
  let embed = embedTextWithUrl (Format.name format <> " Banlists") "https://netrunnerdb.com/en/banlists" $ listRestrictions api format
      colour = if isLatestRestrictionActive format then colNegative else colNeutral
  sendEmbedMessage m "" $ addColour colour embed

-- | @embedBanList@ embeds the list of cards affected by a given banlist.
embedRestriction :: Restriction -> Message -> EnvDatabaseDiscord NrApi ()
embedRestriction r m = do
  api <- ask
  let standard = getStandard api
      (pre, cCards, rCards) = listAffectedCards api r
      header = Restriction.name r <> if isActiveRestriction api standard r then " (active)" else ""
      colour = if isActiveRestriction api standard r then colNegative else colNeutral
  sendEmbedMessage m "" $ addColour colour $ embedColumns header pre [("Corp Cards", cCards), ("Runner Cards", rCards)]
