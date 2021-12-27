{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Tablebot.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the banHistory command.
module Tablebot.Plugins.Netrunner.Command.BanHistory (listBanHistory) where

import Data.Text (Text, intercalate)
import Tablebot.Plugins.Netrunner.Type.BanList (BanList (active))
import qualified Tablebot.Plugins.Netrunner.Type.BanList as BanList
import Tablebot.Plugins.Netrunner.Type.Card (Card)
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Utility.BanList

listBanHistory :: NrApi -> Card -> Text
listBanHistory api card = intercalate "\n" $ map format $ reverse $ banLists api
  where
    format :: BanList -> Text
    format b = symbol b <> " " <> BanList.name b <> formatActive b
    symbol :: BanList -> Text
    symbol b
      | isBanned api b card = "🚫"
      | isRestricted api b card = "🦄"
      | toUniversalInfluence api b card /= 0 = formatNum $ toUniversalInfluence api b card
      | toGlobalPenalty api b card /= 0 = formatNum $ toGlobalPenalty api b card
      | otherwise = "✅"
    formatNum :: Int -> Text
    formatNum 1 = ":one:"
    formatNum 2 = ":two:"
    formatNum 3 = ":three:"
    formatNum 4 = ":four:"
    formatNum 5 = ":five:"
    formatNum 6 = ":six:"
    formatNum 7 = ":seven:"
    formatNum 8 = ":eight:"
    formatNum 9 = ":nine:"
    formatNum _ = ":hash:"
    formatActive :: BanList -> Text
    formatActive b = if active b then " (active)" else ""
