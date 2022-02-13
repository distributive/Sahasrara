-- |
-- Module      : Tablebot.Plugins.Netrunner.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner ban lists in Tablebot.
module Tablebot.Plugins.Netrunner.Utility.BanList where

import Data.Map (lookup)
import Data.Maybe (mapMaybe)
import Tablebot.Plugins.Netrunner.Type.BanList
import Tablebot.Plugins.Netrunner.Type.Card (Card)
import qualified Tablebot.Plugins.Netrunner.Type.Card as Card
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (NrApi, banLists, cards))
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
