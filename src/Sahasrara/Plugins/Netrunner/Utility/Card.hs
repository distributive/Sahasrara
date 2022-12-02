-- |
-- Module      : Sahasrara.Plugins.Netrunner.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cards in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.Card where

import Data.Text (Text, intercalate)
import qualified Data.Text (toTitle)
import Discord.Types
import Safe
import Sahasrara.Plugins.Netrunner.Type.Card (Card (..))
import Sahasrara.Plugins.Netrunner.Type.Faction (Faction)
import qualified Sahasrara.Plugins.Netrunner.Type.Faction as Faction
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Printing (Printing (cardCode))
import Sahasrara.Plugins.Netrunner.Type.Stat (statToText)
import Sahasrara.Plugins.Netrunner.Utility.Faction (toColour)
import Sahasrara.Plugins.Netrunner.Utility.Formatting (formatText)
import Sahasrara.Utility
import Sahasrara.Utility.Types ()

-- | @fromCardCode@ finds a card by its code.
fromCardCode :: NrApi -> Text -> Maybe Card
fromCardCode NrApi {cards = cards} c = case filter ((== c) . code) cards of
  [] -> Nothing
  (x : _) -> Just x

-- | @toTitle@ takes a Netrunner card and attempts to get its title, adding a
-- uniqueness icon if the card is unique.
formatTitle :: Card -> Text
formatTitle card =
  let unique = if uniqueness card then "◆ " else ""
   in unique <> title card

-- | @toPrintings@ gets all printings of a card.
toPrintings :: NrApi -> Card -> [Printing]
toPrintings api card = filter ((== code card) . cardCode) $ printings api

-- | @toLatestPrinting@ gets the most recent printing of a card.
-- Assumes they're ordered oldest first
toLatestPrinting :: NrApi -> Card -> Printing
toLatestPrinting api card = last $ toPrintings api card

-- | @toFaction@ takes a card and attempts to find its faction.
toFaction :: NrApi -> Card -> Faction
toFaction api card =
  let fCode = factionCode card
   in headNote "2" $ filter ((== fCode) . Faction.code) $ factions api

-- | @toFactionColour@ maps cards to their faction's colour.
toFactionColour :: NrApi -> Card -> DiscordColor
toFactionColour api card = toColour $ toFaction api card

-- | @formatCard@ takes a Netrunner card, collects its data and textbox into a
-- single string for displaying in a Discord message/embed.
formatCard :: Card -> EnvDatabaseDiscord NrApi Text
formatCard card = do
  let subtitle = toSubtitle card
  body <- formatText $ text card
  return $ subtitle <> body

-- | @toSubtitle@ generates the first line of a card's embed text listing
-- its types, subtypes, and various other data points.
toSubtitle :: Card -> Text
toSubtitle Card {..} =
  "**"
    <> typeCode
    <> subtypes'
    <> cost'
    <> mu
    <> strength'
    <> agendaStats
    <> trash
    <> influence
    <> deckbuilding
    <> link
    <> "**\n"
  where
    maybeIntToText = maybe "?" intToText
    maybeStatToText = maybe "?" statToText
    maybeEmptyPrependI s mi = maybeEmptyPrepend s (intToText <$> mi)
    maybeEmptyPrependS s mi = maybeEmptyPrepend s (statToText <$> mi)
    typeCode = Data.Text.toTitle $ if cardTypeCode `elem` ["corpIdentity", "runnerIdentity"] then "identity" else cardTypeCode
    subtypes' = ": " <> intercalate " - " subtypes
    cost' =
      let rezText = " • Rez: "
       in case (cost, cardTypeCode) of
            (Nothing, _) -> ""
            (Just x, "asset") -> rezText <> statToText x
            (Just x, "ice") -> rezText <> statToText x
            (Just x, "upgrade") -> rezText <> statToText x
            (Just x, _) -> " • Cost: " <> statToText x
    mu = maybeEmptyPrependI " • MU: " memoryCost
    strength' = maybeEmptyPrependS " • Strength: " strength
    agendaStats =
      let adv = maybeStatToText advancementRequirement
          points = maybeIntToText agendaPoints
       in case cardTypeCode of
            "agenda" -> " • " <> adv <> "/" <> points
            _ -> ""
    trash = maybeEmptyPrependI " • Trash: " trashCost
    influence = case influenceCost of
      Nothing -> ""
      Just x ->
        if x == 0 && cardTypeCode `elem` ["agenda", "identity"]
          then ""
          else " • Influence: " <> intToText x
    deckbuilding = case cardTypeCode of
      "identity" -> " • " <> maybeIntToText minimumDeckSize <> "/" <> maybeIntToText influenceLimit
      _ -> ""
    link = maybeEmptyPrependI " • Link: " baseLink
