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
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Faction (Faction (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi)
import Sahasrara.Utility
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (formatFromEmojiName)
import Sahasrara.Utility.Types ()

-- | @toEmoji@ takes a faction and attempts to find its Discord emoji.
toEmoji :: Faction -> EnvDatabaseDiscord NrApi Text
toEmoji Faction {code = code} = case code of
  "haas_bioroid" -> formatFromEmojiName "s_hb"
  "jinteki" -> formatFromEmojiName "s_jinteki"
  "nbn" -> formatFromEmojiName "s_nbn"
  "weyland_consortium" -> formatFromEmojiName "s_weyland"
  "anarch" -> formatFromEmojiName "s_anarch"
  "criminal" -> formatFromEmojiName "s_criminal"
  "shaper" -> formatFromEmojiName "s_shaper"
  "adam" -> formatFromEmojiName "s_adam"
  "apex" -> formatFromEmojiName "s_apex"
  "sunny_lebeau" -> formatFromEmojiName "s_sunny"
  _ -> formatFromEmojiName "s_nsg"

-- | @toColour@ gets the factional colour of a card to use in its embed.
toColour :: Faction -> DiscordColor
toColour Faction {code = code} = case code of
  "neutral_corp" -> colNeutralCorp
  "haas_bioroid" -> colHB
  "jinteki" -> colJinteki
  "nbn" -> colNBN
  "weyland_consortium" -> colWeyland
  "neutral_runner" -> colNeutralRunner
  "anarch" -> colAnarch
  "criminal" -> colCriminal
  "shaper" -> colShaper
  "adam" -> colAdam
  "apex" -> colApex
  "sunny_lebeau" -> colSunny
  _ -> colError
