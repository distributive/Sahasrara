-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Card and Cards types.
module Sahasrara.Plugins.Netrunner.Type.Legality where

-- @Legality@ represents all the kinds of legality that could affect a card.
-- It assumes a card can never be more than one under a single restriction.
data Legality
  = Legal
  | Rotated -- Was once in the card pool but no longer is
  | Invalid -- Was never in the card pool
  | Banned
  | Restricted
  | UniversalFactionCost Int
  | GlobalPenalty
  | Points Int
  deriving (Eq, Show)
