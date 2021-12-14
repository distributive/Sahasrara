{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Tablebot.Plugin.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The backend functionality of the Netrunner commands.
module Tablebot.Plugin.Netrunner (cardToEmbed, cardToImgEmbed, cardToFlavourEmbed, queryCard) where

import Data.Char (toUpper)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, replace, toLower, toTitle, unpack)
import Discord.Types
import Tablebot.Plugin.Embed (addColour)
import Tablebot.Plugin.Fuzzy (FuzzyCosts (..), closestValueWithCosts)
import Tablebot.Plugin.Netrunner.Card as Card
import Tablebot.Plugin.Netrunner.Cycle as Cycle
import Tablebot.Plugin.Netrunner.Faction as Faction
import Tablebot.Plugin.Netrunner.NrApi (NrApi (..))
import Tablebot.Plugin.Netrunner.Pack as Pack
import Tablebot.Plugin.Types (DiscordColour (Default), hexToDiscordColour)
import Tablebot.Plugin.Utils (intToText)

-- | @queryCard@ fuzzy searches the given library of cards by title.
queryCard :: NrApi -> Text -> Card
queryCard api = closestValueWithCosts editCosts pairs . unpack
  where
    pairs = zip (map (unpack . toLower . (fromMaybe "") . Card.title) $ cards api) $ cards api
    editCosts =
      FuzzyCosts
        { deletion = 10,
          insertion = 2,
          substitution = 10,
          transposition = 1
        }

-- | @cardToLink@ takes a Netrunner card and generates a link to its NetrunnerDB
-- page.
cardToLink :: Card -> Text
cardToLink card = case Card.code card of
  Nothing -> ""
  Just code -> "https://netrunnerdb.com/en/card/" <> code

-- | @cardToImage@ takes a Netrunner card and generates an embed image of the
-- card.
cardToImage :: NrApi -> Card -> Maybe CreateEmbedImage
cardToImage api card = do
  code <- Card.code card
  return $ CreateEmbedImageUrl $ replace "{code}" code $ imageTemplate api

-- | @cardToTitle@ takes a Netrunner card and attempts to get its title, adding
-- a uniqueness icon if the card is unique.
cardToTitle :: Card -> Text
cardToTitle card =
  let unique = if fromMaybe False (uniqueness card) then "◆ " else ""
      cardTitle = fromMaybe "?" $ title card
   in unique <> cardTitle

-- | @cardToText@ takes a Netrunner card, collects its data and textbox into a
-- single string.
cardToText :: Card -> Text
cardToText card =
  let subtitle = cardToSubtitle card
      body = formatText $ fromMaybe "" $ text card
   in subtitle <> body

-- | @cardToSubtitle@ generates the first line of a card's embed text listing
-- its types, subtypes, and various other data points.
cardToSubtitle :: Card -> Text
cardToSubtitle card =
  "**"
    <> type_code
    <> keywords
    <> cost
    <> strength
    <> agendaStats
    <> trash
    <> influence
    <> deckbuilding
    <> link
    <> "**\n"
  where
    type_code :: Text
    type_code = toTitle $ fromMaybe "?" $ Card.type_code card
    keywords :: Text
    keywords = case Card.keywords card of
      Nothing -> ""
      Just t -> ": " <> t
    cost :: Text
    cost =
      let rezText = " • Rez: "
       in case (Card.cost card, Card.type_code card) of
            (Nothing, _) -> ""
            (Just x, Just "asset") -> rezText <> intToText x
            (Just x, Just "ice") -> rezText <> intToText x
            (Just x, Just "upgrade") -> rezText <> intToText x
            (Just x, _) -> " • Cost: " <> intToText x
    strength :: Text
    strength = case Card.strength card of
      Nothing -> ""
      Just x -> " • Strength: " <> intToText x
    agendaStats :: Text
    agendaStats =
      let adv = fromMaybe "?" $ intToText <$> Card.advancement_cost card
          points = fromMaybe "?" $ intToText <$> Card.agenda_points card
       in case Card.type_code card of
            Just "agenda" -> " • " <> adv <> "/" <> points
            _ -> ""
    trash :: Text
    trash = case Card.trash_cost card of
      Nothing -> ""
      Just x -> " • Trash: " <> intToText x
    influence :: Text
    influence = case Card.faction_cost card of
      Nothing -> ""
      Just x ->
        if x == 0 && (fromMaybe "" $ Card.type_code card) `elem` ["agenda", "identity"]
          then ""
          else " • Influence: " <> intToText x
    deckbuilding :: Text
    deckbuilding = case Card.type_code card of
      Just "identity" -> " • " <> ((fromMaybe "?") $ intToText <$> Card.minimum_deck_size card) <> "/" <> ((fromMaybe "?") $ intToText <$> Card.influence_limit card)
      Nothing -> ""
      _ -> ""
    link :: Text
    link = case Card.base_link card of
      Nothing -> ""
      Just x -> " • Link: " <> intToText x

-- | @formatText@ takes a card's raw description and replaces the html
-- formatting tags with Discord formatting. TODO: unhardcode the emoji
formatText :: Text -> Text
formatText raw = foldr (\(f, r) -> replace f r) raw pairs
  where
    pairs :: [(Text, Text)]
    pairs =
      [ ("<strong>", "**"),
        ("</strong>", "**"),
        ("<em>", "*"),
        ("</em>", "*"),
        ("<trace>", "**"),
        ("</trace>", "**"),
        ("[credit]", "<:credit:920005658873581568>"),
        ("[click]", "<:click:920005659142029332>"),
        ("[recurring-credit]", "<:recurring_credit:920005659045548032>"),
        ("[subroutine]", "<:subroutine:920005658865180674>"),
        ("[trash]", "<:trash:920005659280433152>"),
        ("[link]", "<:link:920005658638712833>"),
        ("[mu]", "<:mu:920005658651279401>"),
        ("[haas-bioroid]", "<:hb:920007442681704539>"),
        ("[jinteki]", "<:jinteki:920007443000459354>"),
        ("[nbn]", "<:nbn:920007442669133825>"),
        ("[weyland-consortium]", "<:weyland:920007443331842079>")
      ]

-- | @cardToFaction@ takes a card and attempts to find its faction.
cardToFaction :: NrApi -> Card -> Maybe Faction
cardToFaction api card = do
  faction <- Card.faction_code card
  let fRes = filter (\f -> Faction.code f == faction) $ factions api
  case fRes of
    [] -> Nothing
    (f : _) -> Just f

-- | @cardToPack@ takes a card and attempts to find its data pack.
cardToPack :: NrApi -> Card -> Maybe Pack
cardToPack api card = do
  cardPack <- Card.pack_code card
  let pRes = filter (\p -> Pack.code p == cardPack) $ packs api
  case pRes of
    [] -> Nothing
    (p : _) -> Just p

-- | @packToCycle@ takes a pack and attempts to find its cycle.
packToCycle :: NrApi -> Pack -> Maybe Cycle
packToCycle api pack =
  let cRes = filter (\c -> Cycle.code c == Pack.cycle_code pack) $ cycles api
   in case cRes of
        [] -> Nothing
        (c : _) -> Just c

-- | @cardToReleaseData@ checks if a card was released in a data pack or a big
-- box, and simplifies this info in the case of the latter.
cardToReleaseData :: NrApi -> Card -> Text
cardToReleaseData api card = fromMaybe "" helper
  where
    helper :: Maybe Text
    helper = do
      f <- cardToFaction api card
      p <- cardToPack api card
      c <- packToCycle api p
      let faction = Faction.name f
      let rotation =
            if Cycle.rotated c
              then " (rotated)"
              else ""
      let expansion =
            if Pack.name p == Cycle.name c
              then Pack.name p
              else Cycle.name c <> rotation <> " • " <> Pack.name p
      let position = fromMaybe "" $ (\t -> " #" <> intToText t) <$> Card.position card
      return $ faction <> " • " <> expansion <> position

-- | @cardToColour@ gets the factional colour of a card to use in its embed.
cardToColour :: NrApi -> Card -> DiscordColour
cardToColour api card = fromMaybe Default helper
  where
    helper :: Maybe DiscordColour
    helper = do
      f <- cardToFaction api card
      return $ hexToDiscordColour $ unpack $ Faction.color f

-- | @cardToFlavour@ gets a cards flavour text (and makes it italic).
cardToFlavour :: Card -> Text
cardToFlavour card = case flavor card of
  Nothing -> ""
  Just f -> "*" <> f <> "*"

-- | @cardToLink@ takes a Netrunner card and generates an embed message
-- representing it.
cardToEmbed :: NrApi -> Card -> Embed
cardToEmbed api card =
  let eTitle = cardToTitle card
      eURL = cardToLink card
      eText = cardToText card
      eFoot = cardToReleaseData api card
      eImg = cardToImage api card
      eColour = cardToColour api card
   in addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg eText [] Nothing eFoot Nothing

-- | @cardToImgEmbed@ takes a Netrunner card and attempts to embed a picture of
-- it.
cardToImgEmbed :: NrApi -> Card -> Maybe Embed
cardToImgEmbed api card =
  let eTitle = cardToTitle card
      eURL = cardToLink card
      eColour = cardToColour api card
   in case cardToImage api card of
        Nothing -> Nothing
        eImg -> Just $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL Nothing "" [] eImg "" Nothing

-- | @cardToFlavourEmbed@ takes a Netrunner card and attempts to embed its
-- flavour text.
cardToFlavourEmbed :: NrApi -> Card -> Maybe Embed
cardToFlavourEmbed api card =
  let eTitle = cardToTitle card
      eURL = cardToLink card
      eColour = cardToColour api card
      eImg = cardToImage api card
   in case cardToFlavour card of
        "" -> Nothing
        eFlavour -> Just $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg eFlavour [] Nothing "" Nothing
