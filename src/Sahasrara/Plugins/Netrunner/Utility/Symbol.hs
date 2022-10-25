-- |
-- Module      : Sahasrara.Plugins.Netrunner.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Symbols used by Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.Symbol where

import Data.Text (Text)
import Sahasrara.Plugins.Netrunner.Type.Legality (Legality (..))

symbolLegal :: Text
symbolLegal = "✅"

symbolBanned :: Text
symbolBanned = "🚫"

symbolRotated :: Text
symbolRotated = "🔁"

symbolInvalid :: Text
symbolInvalid = "🔒"

symbolRestricted :: Text
symbolRestricted = "🦄"

symbolGlobalPenalty :: Text
symbolGlobalPenalty = "*️⃣"

-- | @legalityToSymbol@ gets the emoji corresponding to each type of card restriction.
legalityToSymbol :: Legality -> Text
legalityToSymbol Banned = symbolBanned
legalityToSymbol Rotated = symbolRotated
legalityToSymbol Invalid = symbolInvalid
legalityToSymbol Restricted = symbolRestricted
legalityToSymbol (UniversalFactionCost x) = intToSymbol x
legalityToSymbol GlobalPenalty = symbolGlobalPenalty
legalityToSymbol (Points x) = intToSymbol x
legalityToSymbol _ = symbolLegal

-- | @intToSymbol@ formats a number to its Discord emoji.
intToSymbol :: Int -> Text
intToSymbol 0 = "0️⃣"
intToSymbol 1 = "1️⃣"
intToSymbol 2 = "2️⃣"
intToSymbol 3 = "3️⃣"
intToSymbol 4 = "4️⃣"
intToSymbol 5 = "5️⃣"
intToSymbol 6 = "6️⃣"
intToSymbol 7 = "7️⃣"
intToSymbol 8 = "8️⃣"
intToSymbol 9 = "9️⃣"
intToSymbol _ = "#️⃣"
