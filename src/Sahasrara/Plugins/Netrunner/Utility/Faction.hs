-- |
-- Module      : Sahasrara.Plugins.Netrunner.Faction
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner factions in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.Faction where

import Data.Text
import Sahasrara.Plugins.Netrunner.Type.Faction (Faction (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi)
import Sahasrara.Utility
import Sahasrara.Utility.Discord (formatFromEmojiName)
import Sahasrara.Utility.Types ()

-- | @toEmoji@ takes a faction and attempts to find its Discord emoji.
toEmoji :: Faction -> EnvDatabaseDiscord NrApi Text
toEmoji Faction {code = code} = case code of
  "haas-bioroid" -> formatFromEmojiName "s_hb"
  "jinteki" -> formatFromEmojiName "s_jinteki"
  "nbn" -> formatFromEmojiName "s_nbn"
  "weyland-consortium" -> formatFromEmojiName "s_weyland"
  "anarch" -> formatFromEmojiName "s_anarch"
  "criminal" -> formatFromEmojiName "s_criminal"
  "shaper" -> formatFromEmojiName "s_shaper"
  "adam" -> formatFromEmojiName "s_adam"
  "apex" -> formatFromEmojiName "s_apex"
  "sunny-lebeau" -> formatFromEmojiName "s_sunny"
  _ -> formatFromEmojiName "s_nisei"
