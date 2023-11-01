-- |
-- Description : Handles the representation of card legality in Netrunner.
module Sahasrara.Plugins.Netrunner.Utility.Legality where

import Data.List (groupBy, nub, nubBy, sortBy)
import Data.Map (Map, findWithDefault, keys, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, intercalate, toLower)
import qualified Data.Text as T
import Sahasrara.Plugins.Netrunner.Type.Card (Card (Card, sideCode, subtypes, title))
import qualified Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle)
import qualified Sahasrara.Plugins.Netrunner.Type.CardCycle as CardCycle
import Sahasrara.Plugins.Netrunner.Type.CardPool (CardPool (cardCodes, cardCycleCodes))
import Sahasrara.Plugins.Netrunner.Type.Format (Format)
import qualified Sahasrara.Plugins.Netrunner.Type.Format as Format
import Sahasrara.Plugins.Netrunner.Type.Legality
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi)
import Sahasrara.Plugins.Netrunner.Type.Restriction hiding (dateStart)
import qualified Sahasrara.Plugins.Netrunner.Type.Restriction as Restriction
import Sahasrara.Plugins.Netrunner.Type.Snapshot (Snapshot (dateStart, restrictionCode))
import Sahasrara.Plugins.Netrunner.Utility.Card (fromCardCode)
import Sahasrara.Plugins.Netrunner.Utility.Format
import Sahasrara.Plugins.Netrunner.Utility.Snapshot
import Sahasrara.Plugins.Netrunner.Utility.Symbol
import Prelude hiding (lookup)

-- | @toLegality@ gets the legality of a card under a given restriction.
toLegality :: NrApi -> Snapshot -> Card -> Legality
toLegality api snapshot card
  | isRotated api snapshot card = Rotated
  | isInvalid api snapshot card = Invalid
  | isBanned restriction card = Banned
  | isRestricted restriction card = Restricted
  | toUniversalFactionCost restriction card /= 0 = UniversalFactionCost $ toUniversalFactionCost restriction card
  | hasGlobalPenalty restriction card = GlobalPenalty
  | toPoints restriction card /= 0 = Points $ toPoints restriction card
  | otherwise = Legal
  where
    restriction = fromMaybe defaultRestriction $ toRestriction api snapshot

-- | @toLegalitySimple@ doesn't check if a card is part of any card pool.
-- i.e. it just gets the legality based on only a given restriction
toLegalitySimple :: Restriction -> Card -> Legality
toLegalitySimple restriction card
  | isBanned restriction card = Banned
  | isRestricted restriction card = Restricted
  | toUniversalFactionCost restriction card /= 0 = UniversalFactionCost $ toUniversalFactionCost restriction card
  | hasGlobalPenalty restriction card = GlobalPenalty
  | toPoints restriction card /= 0 = Points $ toPoints restriction card
  | otherwise = Legal

-- | @isLegal@ determines if a card is unaffected by a snapshot's restriction.
isLegal :: NrApi -> Snapshot -> Card -> Bool
isLegal api snapshot card = toLegality api snapshot card == Legal

-- | @isLegalSimple@ determines if a card is unaffected by a restriction.
isLegalSimple :: Restriction -> Card -> Bool
isLegalSimple restriction card = toLegalitySimple restriction card == Legal

-- | @isRotated@ determines if a card has rotated under a restriction.
-- True iff the card used to be in the format's card pool but no longer is
isRotated :: NrApi -> Snapshot -> Card -> Bool
isRotated api snapshot card =
  let illegalNow = not $ elem (Card.code card) $ cardCodes $ toCardPool api snapshot
      wasLegal = not $ isInvalid api snapshot card
   in illegalNow && wasLegal

-- | @isInvalid@ determines if a card was never in a format's card pool.
-- Only considers snapshots up to the given snapshot
isInvalid :: NrApi -> Snapshot -> Card -> Bool
isInvalid api snapshot card =
  let format = fromSnapshot api snapshot
      snapshots = filter ((dateStart snapshot >=) . dateStart) $ toSnapshots api format
   in all (not . (elem (Card.code card)) . cardCodes . (toCardPool api)) snapshots

-- | @isBanned@ determines if a card is banned under a restriction.
isBanned :: Restriction -> Card -> Bool
isBanned restriction card = elem (Card.code card) $ banned restriction

-- | @isRestricted@ determines if a card is restricted under a restriction.
isRestricted :: Restriction -> Card -> Bool
isRestricted restriction card = elem (Card.code card) $ restricted restriction

-- | @toUniversalFactionCost@ gets a card's universal cost under a restriction.
toUniversalFactionCost :: Restriction -> Card -> Int
toUniversalFactionCost restriction Card {Card.code = code} = findWithDefault 0 code $ universalFactionCost restriction

-- | @hasGlobalPenalty@ determines if a card had a global penalty under a restriction.
hasGlobalPenalty :: Restriction -> Card -> Bool
hasGlobalPenalty restriction card = elem (Card.code card) $ globalPenalty restriction

-- | @toPoints@ gets a card's points under a restriction.
toPoints :: Restriction -> Card -> Int
toPoints restriction Card {Card.code = code} = findWithDefault 0 code $ points restriction

-- | @cycleLegality@ gets the legality of a given cycle under a given snapshot.
cycleLegality :: NrApi -> Snapshot -> CardCycle -> Legality
cycleLegality api snapshot c
  | cycleRotated api snapshot c = Rotated
  | cycleInvalid api snapshot c = Invalid
  | otherwise = Legal

-- | @isLegal@ determines if a card is unaffected by a snapshot's restriction.
cycleLegal :: NrApi -> Snapshot -> CardCycle -> Bool
cycleLegal api snapshot c = cycleLegality api snapshot c == Legal

-- | @cycleRotated@ determines if a cycle has rotated under a snapshot.
-- True iff the card used to be in the format's card pool but no longer is
cycleRotated :: NrApi -> Snapshot -> CardCycle -> Bool
cycleRotated api snapshot c =
  let illegalNow = not $ elem (CardCycle.code c) $ cardCycleCodes $ toCardPool api snapshot
      wasLegal = not $ cycleInvalid api snapshot c
   in illegalNow && wasLegal

-- | @cycleInvalid@ determines if a cycle was never in a format's card pool.
-- Only considers snapshots up to the given snapshot
cycleInvalid :: NrApi -> Snapshot -> CardCycle -> Bool
cycleInvalid api snapshot c =
  let format = fromSnapshot api snapshot
      snapshots = filter ((dateStart snapshot >=) . dateStart) $ toSnapshots api format
   in all (not . (elem (CardCycle.code c)) . cardCycleCodes . (toCardPool api)) snapshots

-- | @listRestrictions@ lists all restrictions from a given format from oldest
-- to neweset.
listRestrictions :: NrApi -> Format -> Text
listRestrictions api format =
  case reverse $ toRestrictions api format of
    [] -> (Format.name format) <> " doesn't have any banlists!"
    rs -> intercalate "\n" $ map toText rs
  where
    toText :: Restriction -> Text
    toText r = "• " <> Restriction.name r <> if isActiveRestriction api format r then " (active)" else ""

-- | @listHistory@ lists each restriction of the given format and the state of
-- the given card under each version.
listHistory :: NrApi -> Format -> Card -> Text
listHistory api format card =
  if all (\s -> Legal == toLegality api s card) snapshots
    then (legalityToSymbol Legal) <> " This card has never appeared on a list."
    else intercalate "\n" condensed
  where
    snapshots :: [Snapshot] -- Doesn't use toRestrictions because we need the snapshots for their card pools
    snapshots = reverse $ filter (\s -> restrictionCode s /= Nothing && toLegality api s card /= Invalid) $ nubBy (\a b -> restrictionCode a == restrictionCode b) $ toSnapshots api format
    groups :: [[Snapshot]]
    groups = groupBy (\a b -> toLegality api a card == toLegality api b card) snapshots
    condensed :: [Text]
    condensed = concatMap takeFirstLast groups
    takeFirstLast :: [Snapshot] -> [Text]
    takeFirstLast xs =
      if length xs < 4
        then toText <$> xs
        else [toText $ head xs, formatSkip xs, toText $ last xs]
    formatSkip :: [Snapshot] -> Text -- Should never be given a list with fewer than 4 elements
    formatSkip [] = "`#ERROR`"
    formatSkip (x : xs) = legalityToSymbol (toLegality api x card) <> " _unchanged " <> (T.pack $ show $ length xs - 1) <> " updates_"
    toText :: Snapshot -> Text
    toText snapshot =
      let restriction = fromMaybe defaultRestriction $ toRestriction api snapshot
       in legalityToSymbol (toLegality api snapshot card) <> " " <> formatActive restriction
    formatActive :: Restriction -> Text
    formatActive r =
      if isActiveRestriction api format r
        then "**" <> Restriction.name r <> " (active)**"
        else Restriction.name r

-- | @affectedCards@ gets all cards affected by a given restriction.
affectedCards :: NrApi -> Restriction -> [Card]
affectedCards api r = mapMaybe (fromCardCode api) $ nub codes
  where
    codes :: [Text]
    codes = concat [banned r, restricted r, sortBy (valueOf $ universalFactionCost r) $ keys (universalFactionCost r), globalPenalty r, sortBy (valueOf $ points r) $ keys (points r)]
    valueOf :: Map Text Int -> Text -> Text -> Ordering
    valueOf mapping a b =
      let vA = lookup a mapping
          vB = lookup b mapping
       in if vA > vB then LT else if vA < vB then GT else EQ

-- | @listAffectedCards@ lists all the cards affected by a restriction.
-- The output is (additional text, corp cards, runner cards).
-- This is to account for large groups of cards being banned together resulting
-- in otherwise excessively long lists of banned cards.
listAffectedCards :: NrApi -> Restriction -> (Text, [Text], [Text])
listAffectedCards api r =
  let cards = filter (not . (any ((`elem` (bannedSubtypes r)) . toLower)) . subtypes) $ affectedCards api r
      cCards = filter ((== "corp") . sideCode) cards
      rCards = filter ((== "runner") . sideCode) cards
      pre = intercalate "\n" $ map formatSubtype $ bannedSubtypes r
   in (pre, map toText cCards, map toText rCards)
  where
    toText :: Card -> Text
    toText card = legalityToSymbol (toLegalitySimple r card) <> " " <> condense (title card)
    condense :: Text -> Text
    condense t =
      if T.length t > 30
        then T.take 27 t <> "..."
        else t
    formatSubtype :: Text -> Text
    formatSubtype subtype = symbolBanned <> " All cards with the [" <> subtype <> "](https://netrunnerdb.com/find/?q=s%3A" <> toLower subtype <> ") subtype."
