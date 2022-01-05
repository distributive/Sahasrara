-- |
-- Module      : Tablebot.Plugins.Netrunner.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cards in Tablebot.
module Tablebot.Plugins.Netrunner.Utility.Card
  ( toTitle,
    toText,
    toLink,
    toImage,
    toSubtitle,
    toFaction,
    toPack,
    toCycle,
    toReleaseData,
    toColour,
    toFlavour,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, replace, unpack)
import qualified Data.Text (toTitle)
import Discord.Types
import Tablebot.Plugins.Netrunner.Type.Card (Card (..))
import Tablebot.Plugins.Netrunner.Type.Cycle (Cycle)
import qualified Tablebot.Plugins.Netrunner.Type.Cycle as Cycle
import Tablebot.Plugins.Netrunner.Type.Faction (Faction)
import qualified Tablebot.Plugins.Netrunner.Type.Faction as Faction
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Type.Pack (Pack (cycleCode))
import qualified Tablebot.Plugins.Netrunner.Type.Pack as Pack
import Tablebot.Plugins.Netrunner.Utility.BanList (activeBanList, isBanned, isRestricted, toGlobalPenalty, toUniversalInfluence)
import Tablebot.Plugins.Netrunner.Utility.Misc (formatNr)
import Tablebot.Utility
import Tablebot.Utility.Types ()
import Tablebot.Utility.Utils (intToText, maybeEmptyPrepend)

-- | @toLink@ takes a card and generates a link to its NetrunnerDB page.
toLink :: Card -> Text
toLink card = maybeEmptyPrepend "https://netrunnerdb.com/en/card/" $ code card

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
    <> typeCode'
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
    typeCode' = maybe "?" Data.Text.toTitle typeCode
    keywords' = maybeEmptyPrepend ": " keywords
    cost' =
      let rezText = " • Rez: "
       in case (cost, typeCode) of
            (Nothing, _) -> ""
            (Just x, Just "asset") -> rezText <> intToText x
            (Just x, Just "ice") -> rezText <> intToText x
            (Just x, Just "upgrade") -> rezText <> intToText x
            (Just x, _) -> " • Cost: " <> intToText x
    mu = maybeEmptyPrependI " • MU: " memoryCost
    strength' = maybeEmptyPrependI " • Strength: " strength
    agendaStats =
      let adv = maybeIntToText advancementCost
          points = maybeIntToText agendaPoints
       in case typeCode of
            Just "agenda" -> " • " <> adv <> "/" <> points
            _ -> ""
    trash = maybeEmptyPrependI " • Trash: " trashCost
    influence = case factionCost of
      Nothing -> ""
      Just x ->
        if x == 0 && fromMaybe "" typeCode `elem` ["agenda", "identity"]
          then ""
          else " • Influence: " <> intToText x
    deckbuilding = case typeCode of
      Just "identity" -> " • " <> maybeIntToText minimumDeckSize <> "/" <> maybeIntToText influenceLimit
      Nothing -> ""
      _ -> ""
    link = maybeEmptyPrependI " • Link: " baseLink

-- | @toFaction@ takes a card and attempts to find its faction.
toFaction :: NrApi -> Card -> Maybe Faction
toFaction api card = do
  cardCode <- factionCode card
  let fRes = filter ((== cardCode) . Faction.code) $ factions api
  case fRes of
    [] -> Nothing
    (f : _) -> Just f

-- | @toPack@ takes a card and attempts to find its data pack.
toPack :: NrApi -> Card -> Maybe Pack
toPack api card = do
  pCode <- packCode card
  let pRes = filter ((== pCode) . Pack.code) $ packs api
  case pRes of
    [] -> Nothing
    (p : _) -> Just p

-- | @toCycle@ takes a card and attempts to find its cycle.
toCycle :: NrApi -> Card -> Maybe Cycle
toCycle api card = do
  cCode <- cycleCode <$> toPack api card
  let cRes = filter ((== cCode) . Cycle.code) $ cycles api
  case cRes of
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
      c <- toCycle api card
      let faction = Faction.name f
          rotation = if Cycle.rotated c then " (rotated)" else ""
          banList = activeBanList api
          banStatus = if isBanned api banList card then " (banned)" else ""
          restriction = if isRestricted api banList card then " (restricted)" else ""
          globalPenalty = case toGlobalPenalty api banList card of
            0 -> ""
            x -> " (global penalty: " <> pack (show x) <> ")"
          universalInf = case toUniversalInfluence api banList card of
            0 -> ""
            x -> " (universal influence: " <> pack (show x) <> ")"
          legality = rotation <> banStatus <> restriction <> globalPenalty <> universalInf
          expansion =
            Cycle.name c <> legality
              <> if Pack.name p /= Cycle.name c
                then " • " <> Pack.name p
                else ""
          pos = maybe "" (\t -> " #" <> intToText t) (position card)
      return $ faction <> " • " <> expansion <> pos

-- | @toColour@ gets the factional colour of a card to use in its embed.
toColour :: NrApi -> Card -> DiscordColour
toColour api card = maybe Default (hexToDiscordColour . unpack . Faction.colour) (toFaction api card)

-- | @toFlavour@ gets a cards flavour text (and makes it italic).
toFlavour :: Card -> EnvDatabaseDiscord NrApi (Maybe Text)
toFlavour Card {flavour = flavour} = case flavour of
  Nothing -> return Nothing
  Just f -> do
    f' <- formatNr f
    return $ Just $ "*" <> f' <> "*"
