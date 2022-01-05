-- |
-- Module      : Tablebot.Plugins.Netrunner.Faction
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner factions in Tablebot.
module Tablebot.Plugins.Netrunner.Utility.Faction where

import Data.Text
import Tablebot.Plugins.Netrunner.Type.Faction (Faction (..))
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi)
import Tablebot.Utility
import Tablebot.Utility.Discord (formatFromEmojiName)
import Tablebot.Utility.Types ()

-- | @toEmoji@ takes a faction and attempts to find its Discord emoji.
toEmoji :: Faction -> EnvDatabaseDiscord NrApi Text
toEmoji Faction {code = code} = case code of
  "haas-bioroid" -> formatFromEmojiName "hb"
  "jinteki" -> formatFromEmojiName "jinteki"
  "nbn" -> formatFromEmojiName "nbn"
  "weyland-consortium" -> formatFromEmojiName "weyland"
  "anarch" -> formatFromEmojiName "anarch"
  "criminal" -> formatFromEmojiName "criminal"
  "shaper" -> formatFromEmojiName "shaper"
  "adam" -> formatFromEmojiName "adam"
  "apex" -> formatFromEmojiName "apex"
  "sunny-lebeau" -> formatFromEmojiName "sunny"
  _ -> formatFromEmojiName "nisei"
