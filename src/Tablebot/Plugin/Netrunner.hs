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

import Data.Maybe (fromMaybe)
import Data.Text (Text, replace, toLower, toTitle, unpack, isInfixOf)
import Discord.Types
import Tablebot.Plugin
import Tablebot.Plugin.Discord (formatFromEmojiName)
import Tablebot.Plugin.Embed (addColour)
import Tablebot.Plugin.Fuzzy (FuzzyCosts (..), closestValueWithCosts)
import Tablebot.Plugin.Netrunner.Card as Card (Card (..))
import Tablebot.Plugin.Netrunner.Cycle as Cycle (Cycle (..))
import Tablebot.Plugin.Netrunner.Faction as Faction (Faction (..))
import Tablebot.Plugin.Netrunner.NrApi (NrApi (..))
import Tablebot.Plugin.Netrunner.Pack as Pack (Pack (..))
import Tablebot.Plugin.Types ()
import Tablebot.Plugin.Utils (intToText)

-- | @queryCard@ searches the given library of cards by title, first checking if
-- the search query is a substring of any cards, then performing a fuzzy search on
-- the cards given, or all of the cards if no cards are found
queryCard :: NrApi -> Text -> Card
queryCard NrApi {cards = cards} txt = findCard (substringSearch pairs txt) txt pairs
  where
    pairs = zip (map (toLower . fromMaybe "" . Card.title) cards) cards
    substringSearch thePairs searchTxt = filter (\(x, _) -> isInfixOf (toLower searchTxt) x) thePairs


-- | @findCard finds a card from the given list of pairs that is some subset of a
-- full list. If the sublist is empty, it will fuzzy search the full list. If the sublist
-- has exactly 1 element, it'll return that element. If the sublist has multiple
-- elements, it will fuzzy search the sublist
findCard :: [(Text, Card)] -> Text -> [(Text, Card)] -> Card
findCard [] searchTxt fullPairs = fuzzyQueryCard fullPairs searchTxt
findCard [(_, card)] _ _ = card
findCard pairs searchTxt _ = fuzzyQueryCard pairs searchTxt


-- | @queryCard@ fuzzy searches the given library of cards by title.
fuzzyQueryCard :: [(Text, Card)] -> Text -> Card
fuzzyQueryCard pairs = closestValueWithCosts editCosts unpackedPairs . unpack
  where
    unpackedPairs  = fmap (\(x, y) -> (unpack x, y)) pairs
    editCosts =
      FuzzyCosts
        { deletion = 10,
          insertion = 2,
          substitution = 10,
          transposition = 1
        }

-- | Utility function to prepend a given Text to Text within a Maybe, or return the empty
-- Text.
maybeEmptyPrepend :: Text -> Maybe Text -> Text
maybeEmptyPrepend s = maybe "" (s <>)

-- | @cardToLink@ takes a Netrunner card and generates a link to its NetrunnerDB
-- page.
cardToLink :: Card -> Text
cardToLink card = maybeEmptyPrepend "https://netrunnerdb.com/en/card/" (Card.code card)

-- | @cardToImage@ takes a Netrunner card and generates an embed image of the
-- card.
cardToImage :: NrApi -> Card -> Maybe CreateEmbedImage
cardToImage api card = do
  code' <- Card.code card
  return $ CreateEmbedImageUrl $ replace "{code}" code' $ imageTemplate api

-- | @cardToTitle@ takes a Netrunner card and attempts to get its title, adding
-- a uniqueness icon if the card is unique.
cardToTitle :: Card -> Text
cardToTitle card =
  let unique = if Just True == uniqueness card then "◆ " else ""
      cardTitle = fromMaybe "?" $ title card
   in unique <> cardTitle

-- | @cardToText@ takes a Netrunner card, collects its data and textbox into a
-- single string.
cardToText :: Card -> EnvDatabaseDiscord NrApi Text
cardToText card = do
  let subtitle = cardToSubtitle card
  body <- formatText (fromMaybe "" $ text card)
  return $ subtitle <> body

-- | @cardToSubtitle@ generates the first line of a card's embed text listing
-- its types, subtypes, and various other data points.
cardToSubtitle :: Card -> Text
cardToSubtitle Card {..} =
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
    type_code' = maybe "?" toTitle type_code
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

-- | @formatText@ takes a card's raw description and replaces the html
-- formatting tags with Discord formatting.
formatText :: Text -> EnvDatabaseDiscord NrApi Text
formatText raw = do
  credit <- formatFromEmojiName "credit"
  click <- formatFromEmojiName "click"
  recurringCredit <- formatFromEmojiName "recurring_credit"
  subroutine <- formatFromEmojiName "subroutine"
  trash <- formatFromEmojiName "trash_ability"
  link <- formatFromEmojiName "link"
  mu <- formatFromEmojiName "mu"
  hb <- formatFromEmojiName "hb"
  jinteki <- formatFromEmojiName "jinteki"
  nbn <- formatFromEmojiName "nbn"
  weyland <- formatFromEmojiName "weyland"
  anarch <- formatFromEmojiName "anarch"
  criminal <- formatFromEmojiName "criminal"
  shaper <- formatFromEmojiName "shaper"
  apex <- formatFromEmojiName "apex"
  adam <- formatFromEmojiName "adam"
  sunny <- formatFromEmojiName "sunny"
  return $
    foldr (uncurry replace) raw $
      [ ("<strong>", "**"),
        ("</strong>", "**"),
        ("<em>", "*"),
        ("</em>", "*"),
        ("<trace>", "**"),
        ("</trace>", "**"),
        ("[credit]", credit),
        ("[click]", click),
        ("[recurring-credit]", recurringCredit),
        ("[subroutine]", subroutine),
        ("[trash]", trash),
        ("[link]", link),
        ("[mu]", mu),
        ("[haas-bioroid]", hb),
        ("[jinteki]", jinteki),
        ("[nbn]", nbn),
        ("[weyland-consortium]", weyland),
        ("[anarch]", anarch),
        ("[criminal]", criminal),
        ("[shaper]", shaper),
        ("[apex]", apex),
        ("[adam]", adam),
        ("[sunny-lebeau]", sunny)
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
      let position = maybe "" (\t -> " #" <> intToText t) (Card.position card)
      return $ faction <> " • " <> expansion <> position

-- | @cardToColour@ gets the factional colour of a card to use in its embed.
cardToColour :: NrApi -> Card -> DiscordColour
cardToColour api card = maybe Default (hexToDiscordColour . unpack . Faction.color) (cardToFaction api card)

-- | @cardToFlavour@ gets a cards flavour text (and makes it italic).
cardToFlavour :: Card -> Text
cardToFlavour card = maybe "" (\f -> "*" <> f <> "*") (Card.flavor card)

-- | @cardToLink@ takes a Netrunner card and generates an embed message
-- representing it.
cardToEmbed :: NrApi -> Card -> EnvDatabaseDiscord NrApi Embed
cardToEmbed api card = do
  let eTitle = cardToTitle card
  let eURL = cardToLink card
  eText <- cardToText card
  let eFoot = cardToReleaseData api card
  let eImg = cardToImage api card
  let eColour = cardToColour api card
  return $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg eText [] Nothing eFoot Nothing Nothing

-- | @cardToImgEmbed@ takes a Netrunner card and attempts to embed a picture of
-- it.
cardToImgEmbed :: NrApi -> Card -> Maybe Embed
cardToImgEmbed api card =
  let eTitle = cardToTitle card
      eURL = cardToLink card
      eColour = cardToColour api card
   in case cardToImage api card of
        Nothing -> Nothing
        eImg -> Just $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL Nothing "" [] eImg "" Nothing Nothing

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
        eFlavour -> Just $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg eFlavour [] Nothing "" Nothing Nothing
