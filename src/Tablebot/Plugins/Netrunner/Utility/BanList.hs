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

-- | @toBanEntries@ finds all ban entries for all versions of a given card to
-- account for reprints.
-- NOTE: Assumes two cards with the same title are the same card.
toBanEntries :: NrApi -> BanList -> Card -> [CardBan]
toBanEntries NrApi {cards = cards} bl card =
  let reprints = filter ((Card.title card ==) . Card.title) cards
      codes = mapMaybe Card.code reprints
   in mapMaybe (\c -> lookup c $ affectedCards bl) codes

-- | @toGlobalPenalty@ gets a cards global penalty under a given ban list.
toGlobalPenalty :: NrApi -> BanList -> Card -> Int
toGlobalPenalty api bl c = case mapMaybe globalPenalty $ toBanEntries api bl c of
  [] -> 0
  xs -> maximum xs

-- | @toUniversalInfluence@ gets a cards universal influence under a ban list.
toUniversalInfluence :: NrApi -> BanList -> Card -> Int
toUniversalInfluence api bl c = case mapMaybe universalInfluence $ toBanEntries api bl c of
  [] -> 0
  xs -> maximum xs

-- | @isBanned@ determines if a card is banned under a given ban list.
isBanned :: NrApi -> BanList -> Card -> Bool
isBanned api bl c = case mapMaybe banned $ toBanEntries api bl c of
  [] -> False
  xs -> or xs

-- | @isRestricted@ determines if a card is restricted under a given ban list.
isRestricted :: NrApi -> BanList -> Card -> Bool
isRestricted api bl c = case mapMaybe restricted $ toBanEntries api bl c of
  [] -> False
  xs -> or xs
