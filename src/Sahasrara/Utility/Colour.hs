-- |
-- Module      : Sahasrara.Utility.Colour
-- Description : Defines the Sahasrara colours in one place.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Sahasrara's colour scheme.
module Sahasrara.Utility.Colour where

import Discord.Types (DiscordColor (..))

colPositive, colNeutral, colNegative, colHelp, colInfo, colError, colHQ, colRnD, colArchives, colHoroscope, colRoll, colCommon, colUncommon, colRare, colVital :: DiscordColor
-- Positive/neutral/negative
colPositive = DiscordColorDiscordGreen
colNeutral = DiscordColorDiscordYellow
colNegative = DiscordColorDiscordRed
-- Signposting
colHelp = DiscordColorAqua
colInfo = DiscordColorBlue
colError = DiscordColorDarkRed
-- Central servers
colHQ = DiscordColorBlue
colRnD = DiscordColorGreen
colArchives = DiscordColorRed
-- Horoscope
colHoroscope = DiscordColorRGB 170 141 216
-- Roll
colRoll = DiscordColorDiscordWhite
-- ONR rarity
colCommon = DiscordColorRGB 177 150 71
colUncommon = DiscordColorRGB 71 177 80
colRare = DiscordColorRGB 71 93 177
colVital = DiscordColorRGB 145 71 177
