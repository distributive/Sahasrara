{-# OPTIONS_GHC -Wno-missing-signatures #-}

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

-- Positive/neutral/negative
colPositive = DiscordColorDiscordGreen

colNeutral = DiscordColorDiscordYellow

colNegative = DiscordColorDiscordRed

-- Legality
colLegal = colPositive

colRotated = DiscordColorBlue

colInvalid = DiscordColorGray

colBanned = colNegative

colRestricted = DiscordColorPurple

colUniversalFactionCost = DiscordColorPurple

colGlobalPenalty = DiscordColorPurple

colPoints = DiscordColorPurple

-- Signposting
colHelp = DiscordColorAqua

colInfo = DiscordColorBlue

colError = DiscordColorDarkRed

-- Factions
colNeutralCorp = DiscordColorRGB 128 128 128

colHB = DiscordColorRGB 138 43 226

colJinteki = DiscordColorRGB 220 20 60

colNBN = DiscordColorRGB 255 140 0

colWeyland = DiscordColorRGB 0 100 0

colNeutralRunner = DiscordColorRGB 128 128 128

colAnarch = DiscordColorRGB 255 69 0

colCriminal = DiscordColorRGB 65 105 225

colShaper = DiscordColorRGB 50 205 50

colAdam = DiscordColorRGB 167 156 89

colApex = DiscordColorRGB 140 72 71

colSunny = DiscordColorRGB 110 110 110

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
