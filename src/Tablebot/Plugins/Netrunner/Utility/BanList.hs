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

import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import Data.Map (lookup)
import Tablebot.Plugins.Netrunner.Type.BanList
import Tablebot.Plugins.Netrunner.Type.Card (Card)
import qualified Tablebot.Plugins.Netrunner.Type.Card as Card
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (NrApi, banLists))

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

-- | @isBanned@ determines if a card is banned under a given ban list.
isBanned :: BanList -> Card -> Bool
isBanned bl c = fromMaybe False $ do
  cCode <- Card.code c
  bData <- lookup cCode $ affectedCards bl
  banned bData
