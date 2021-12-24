-- |
-- Module      : Tablebot.Plugins.Netrunner.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cards in Tablebot.
module Tablebot.Plugins.Netrunner.Card
  ( toTitle,
    toText,
    toLink,
    toImage,
    toSubtitle,
    toFaction,
    toPack,
    toReleaseData,
    toColour,
    toFlavour,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, replace, unpack)
import Discord.Types
import Tablebot.Plugins.Netrunner.Pack (toCycle)
import Tablebot.Plugins.Netrunner.Type.Card (Card (..))
import qualified Tablebot.Plugins.Netrunner.Type.Cycle as Cycle
import Tablebot.Plugins.Netrunner.Type.Faction (Faction)
import qualified Tablebot.Plugins.Netrunner.Type.Faction as Faction
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Type.Pack (Pack)
import qualified Tablebot.Plugins.Netrunner.Type.Pack as Pack
import Tablebot.Plugins.Netrunner.Utils (formatNr)
import Tablebot.Utility
import Tablebot.Utility.Types ()
import Tablebot.Utility.Utils (intToText, maybeEmptyPrepend)

-- | @toLink@ takes a card and generates a link to its NetrunnerDB page.
toLink :: Card -> Text
toLink card = maybeEmptyPrepend "https://netrunnerdb.com/en/card/" (code card)

-- | @toImage@ takes a Netrunner card and loads an embed image of it.
toImage :: NrApi -> Card -> Maybe CreateEmbedImage
toImage api card = do
  code' <- code card
  return $ CreateEmbedImageUrl $ replace "{code}" code' $ imageTemplate api

-- | @toTitle@ takes a Netrunner card and attempts to get its title, adding
-- a uniqueness icon if the card is unique.
toTitle :: Card -> Text
toTitle card =
  let unique = if Just True == uniqueness card then "◆ " else ""
      cardTitle = fromMaybe "?" $ title card
   in unique <> cardTitle

-- | @toText@ takes a Netrunner card, collects its data and textbox into a
-- single string.
toText :: Card -> EnvDatabaseDiscord NrApi Text
toText card = do
  let subtitle = toSubtitle card
  body <- formatNr (fromMaybe "" $ text card)
  return $ subtitle <> body

-- | @toSubtitle@ generates the first line of a card's embed text listing
-- its types, subtypes, and various other data points.
toSubtitle :: Card -> Text
toSubtitle Card {..} =
  "**"
    <> type_code'
    <> keywords'
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
    maybeEmptyPrependI s mi = maybeEmptyPrepend s (intToText <$> mi)
    type_code' = fromMaybe "?" type_code -- TODO: maybe "?" toTitle type_code
    keywords' = maybeEmptyPrepend ": " keywords
    cost' =
      let rezText = " • Rez: "
       in case (cost, type_code) of
            (Nothing, _) -> ""
            (Just x, Just "asset") -> rezText <> intToText x
            (Just x, Just "ice") -> rezText <> intToText x
            (Just x, Just "upgrade") -> rezText <> intToText x
            (Just x, _) -> " • Cost: " <> intToText x
    mu = maybeEmptyPrependI " • MU: " memory_cost
    strength' = maybeEmptyPrependI " • Strength: " strength
    agendaStats =
      let adv = maybeIntToText advancement_cost
          points = maybeIntToText agenda_points
       in case type_code of
            Just "agenda" -> " • " <> adv <> "/" <> points
            _ -> ""
    trash = maybeEmptyPrependI " • Trash: " trash_cost
    influence = case faction_cost of
      Nothing -> ""
      Just x ->
        if x == 0 && fromMaybe "" type_code `elem` ["agenda", "identity"]
          then ""
          else " • Influence: " <> intToText x
    deckbuilding = case type_code of
      Just "identity" -> " • " <> maybeIntToText minimum_deck_size <> "/" <> maybeIntToText influence_limit
      Nothing -> ""
      _ -> ""
    link = maybeEmptyPrependI " • Link: " base_link

-- | @toFaction@ takes a card and attempts to find its faction.
toFaction :: NrApi -> Card -> Maybe Faction
toFaction api card = do
  cardCode <- faction_code card
  let fRes = filter ((== cardCode) . Faction.code) $ factions api
  case fRes of
    [] -> Nothing
    (f : _) -> Just f

-- | @toPack@ takes a card and attempts to find its data pack.
toPack :: NrApi -> Card -> Maybe Pack
toPack api card = do
  cardPack <- pack_code card
  let pRes = filter ((== cardPack) . Pack.code) $ packs api
  case pRes of
    [] -> Nothing
    (p : _) -> Just p

-- | @toReleaseData@ checks if a card was released in a data pack or a big
-- box, and simplifies this info in the case of the latter.
toReleaseData :: NrApi -> Card -> Text
toReleaseData api card = fromMaybe "" helper
  where
    helper :: Maybe Text
    helper = do
      f <- toFaction api card
      p <- toPack api card
      c <- toCycle api p
      let faction = Faction.name f
      let rotation =
            if Cycle.rotated c
              then " (rotated)"
              else ""
      let expansion =
            if Pack.name p == Cycle.name c
              then Pack.name p
              else Cycle.name c <> rotation <> " • " <> Pack.name p
      let pos = maybe "" (\t -> " #" <> intToText t) (position card)
      return $ faction <> " • " <> expansion <> pos

-- | @toColour@ gets the factional colour of a card to use in its embed.
toColour :: NrApi -> Card -> DiscordColour
toColour api card = maybe Default (hexToDiscordColour . unpack . Faction.colour) (toFaction api card)

-- | @toFlavour@ gets a cards flavour text (and makes it italic).
toFlavour :: Card -> EnvDatabaseDiscord NrApi (Maybe Text)
toFlavour Card {flavor = flavor} = case flavor of
  Nothing -> return Nothing
  Just f -> do
    f' <- formatNr f
    return $ Just $ "*" <> f' <> "*"
