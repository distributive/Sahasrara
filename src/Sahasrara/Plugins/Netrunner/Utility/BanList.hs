-- |
-- Module      : Sahasrara.Plugins.Netrunner.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner ban lists in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.BanList where

import Data.List (nubBy)
import Data.Map (keys, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, intercalate, isInfixOf, toLower)
import qualified Data.Text as T
import Sahasrara.Plugins.Netrunner.Type.BanList
import Sahasrara.Plugins.Netrunner.Type.Card (Card, sideCode, subtypes, title)
import qualified Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (NrApi, banLists, cards))
import Prelude hiding (lookup)

-- | @latestBanList@ gets the latest banlist stored in the api.
-- It assumes banlists are stored oldest to newest
latestBanList :: NrApi -> BanList
latestBanList NrApi {banLists = banLists} = case banLists of
  [] -> defaultBanList
  xs -> last xs

-- | @activeBanList@ gets the currently active banlist from the api.
activeBanList :: NrApi -> BanList
activeBanList NrApi {banLists = banLists} = case filter active banLists of
  [] -> defaultBanList
  xs -> last xs -- Last to ensure it's the most recent active version if there's somehow multiple

-- | @latestBanListActive@ checks if the latest banlist is active.
latestBanListActive :: NrApi -> Bool
latestBanListActive api = latestBanList api == activeBanList api

-- | @toBanEntries@ finds all ban entries for all versions of a given card to
-- account for reprints.
-- NOTE: Assumes two cards with the same title are the same card.
toBanEntries :: NrApi -> BanList -> Card -> [CardBan]
toBanEntries NrApi {cards = cards} bl card =
  let reprints = filter ((Card.title card ==) . Card.title) cards
      codes = mapMaybe Card.code reprints
   in mapMaybe (\c -> lookup c $ affectedCards bl) codes

-- | @toMwlStatus@ checks if any version of a card appears in the given banlist
-- and returns the MWL legality of that card.
toMwlStatus :: NrApi -> BanList -> Card -> CardBan
toMwlStatus api bl c
  | isBanned api bl c = Banned
  | isRestricted api bl c = Restricted
  | toUniversalInfluence api bl c /= 0 = UniversalInfluence $ toUniversalInfluence api bl c
  | toGlobalPenalty api bl c /= 0 = GlobalPenalty $ toGlobalPenalty api bl c
  | otherwise = Legal

-- | @isLegal@ determines if a card is unaffected by a banlist.
isLegal :: NrApi -> BanList -> Card -> Bool
isLegal api bl c = Legal == toMwlStatus api bl c

-- | @isBanned@ determines if a card is banned under a given ban list.
isBanned :: NrApi -> BanList -> Card -> Bool
isBanned api bl c = any (== Banned) $ toBanEntries api bl c

-- | @isRestricted@ determines if a card is restricted under a given ban list.
isRestricted :: NrApi -> BanList -> Card -> Bool
isRestricted api bl c = any (== Restricted) $ toBanEntries api bl c

-- | @toUniversalInfluence@ gets a cards universal influence under a ban list.
toUniversalInfluence :: NrApi -> BanList -> Card -> Int
toUniversalInfluence api bl c = case map ui $ toBanEntries api bl c of
  [] -> 0
  xs -> maximum xs
  where
    ui :: CardBan -> Int
    ui (UniversalInfluence x) = x
    ui _ = 0

-- | @toGlobalPenalty@ gets a cards global penalty under a given ban list.
toGlobalPenalty :: NrApi -> BanList -> Card -> Int
toGlobalPenalty api bl c = case map gp $ toBanEntries api bl c of
  [] -> 0
  xs -> maximum xs
  where
    gp :: CardBan -> Int
    gp (GlobalPenalty x) = x
    gp _ = 0

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
    format b = symbol (toMwlStatus api b card) <> " " <> name b <> formatActive b
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
          then filter (not . ("current" `isInfixOf`) . toLower . (fromMaybe "") . subtypes) allCards
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
    find cCode = case filter ((Just cCode ==) . Card.code) $ cards api of
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
