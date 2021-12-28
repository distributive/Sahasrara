{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Tablebot.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the banHistory and banList commands.
module Tablebot.Plugins.Netrunner.Command.BanList
  ( queryBanList,
    listBanLists,
    listAffectedCards,
    listBanHistory,
  )
where

import Data.List (nubBy)
import Data.Map (keys)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, intercalate, isInfixOf, toLower, unpack)
import qualified Data.Text as T (length, take)
import Tablebot.Plugins.Netrunner.Type.BanList (BanList (active, affectedCards, listId, name), CardBan (..))
import qualified Tablebot.Plugins.Netrunner.Type.BanList as BanList
import Tablebot.Plugins.Netrunner.Type.Card (Card (code, keywords, sideCode, title))
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Utility.BanList
import Tablebot.Utility.Search (FuzzyCosts (..), closestValueWithCosts)

-- | @queryBanList@ matches the input to the banlist with the closest name.
queryBanList :: NrApi -> Text -> BanList
queryBanList api query =
  let bls = ("active", activeBanList api) : ("latest", latestBanList api) : (zip (map (unpack . toLower . name) $ banLists api) $ banLists api)
   in closestValueWithCosts editCosts bls $ unpack $ toLower query
  where
    editCosts =
      FuzzyCosts
        { deletion = 100,
          insertion = 1,
          substitution = 100,
          transposition = 100
        }

-- | @listBanLists@ lists all banlists from Netrunner history.
listBanLists :: NrApi -> Text
listBanLists api = intercalate "\n" $ map format $ reverse $ banLists api
  where
    format :: BanList -> Text
    format b = "• " <> name b <> if active b then " (active)" else ""

-- | @listBanHistory@ lists each version of the Netrunner banlist and the state
-- of the given card under each version.
listBanHistory :: NrApi -> Card -> Text
listBanHistory api card = intercalate "\n" $ map format $ reverse $ banLists api
  where
    format :: BanList -> Text
    format b = symbol (toMwlStatus api b card) <> " " <> BanList.name b <> formatActive b
    formatActive :: BanList -> Text
    formatActive b = if active b then " (active)" else ""

-- | @listAffectedCards@ lists all the cards affected by a banlist with links.
-- The output pair is (additional text, list of linked cards). This is to
-- account for large groups of cards being banned together resulting in
-- otherwise excessively long lists of banned cards.
listAffectedCards :: NrApi -> BanList -> (Text, [Text], [Text])
listAffectedCards api b =
  let banCurrents = listId b > 15 -- All banlists since list _all_ unrotated currents
      allCards = nubBy (\c1 c2 -> title c1 == title c2) $ mapMaybe find $ keys $ affectedCards b
      cards =
        if banCurrents
          then filter (not . ("current" `isInfixOf`) . toLower . (fromMaybe "") . keywords) allCards
          else allCards
      cCards = filter ((== Just "corp") . sideCode) cards
      rCards = filter ((== Just "runner") . sideCode) cards
      pre =
        if banCurrents
          then "🚫 All cards with the [Current](https://netrunnerdb.com/find/?q=s%3Acurrent) subtype."
          else ""
   in (pre, map format cCards, map format rCards)
  where
    find :: Text -> Maybe Card
    find cCode = case filter ((Just cCode ==) . code) $ cards api of
      [] -> Nothing
      xs -> Just $ head xs
    format :: Card -> Text
    format card = symbol (toMwlStatus api b card) <> " " <> condense (fromMaybe "?" $ title card)
    condense :: Text -> Text
    condense t =
      if T.length t > 30
        then T.take 27 t <> "..."
        else t

-- | @symbol@ gets the emoji corresponding to each type of card ban.
symbol :: CardBan -> Text
symbol Banned = "🚫"
symbol Restricted = "🦄"
symbol (UniversalInfluence x) = formatNum x
symbol (GlobalPenalty x) = formatNum x
symbol _ = "✅"

-- | @formatNum@ formats a number to its Discord emoji.
formatNum :: Int -> Text
formatNum 1 = "1️⃣"
formatNum 2 = "2️⃣"
formatNum 3 = "3️⃣"
formatNum 4 = "4️⃣"
formatNum 5 = "5️⃣"
formatNum 6 = "6️⃣"
formatNum 7 = "7️⃣"
formatNum 8 = "8️⃣"
formatNum 9 = "9️⃣"
formatNum 0 = "0️⃣"
formatNum _ = "#️⃣"
