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
module Tablebot.Plugin.Netrunner (cardToEmbed, queryCard) where

import Data.Char (toLower, toUpper)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, replace)
import Discord.Types
import Tablebot.Plugin.Embed (addColour)
import Tablebot.Plugin.Netrunner.NrApi (NrApi (..))
import Tablebot.Plugin.Netrunner.Card as Card
import Tablebot.Plugin.Netrunner.Cycle as Cycle
import Tablebot.Plugin.Netrunner.Faction as Faction
import Tablebot.Plugin.Netrunner.Pack as Pack
import Tablebot.Plugin.Types (DiscordColour (Default), hexToDiscordColour)
import Text.EditDistance -- TODO - replace with Fuzzy plugin

-- | @queryCard@ fuzzy searches the given library of cards by title.
queryCard :: NrApi -> String -> Maybe Card
queryCard api query = Just $ minimumBy comparison $ reverse $ cards api
  where
    comparison :: Card -> Card -> Ordering
    comparison a b
      | score a < score b = LT
      | score a > score b = GT
      | otherwise = EQ
    score card = case Card.title card of
      Nothing -> maxBound
      Just title -> levenshteinDistance editCosts (map toLower query) $ map toLower $ unpack title
    editCosts = EditCosts {
      deletionCosts = ConstantCost 10,
      insertionCosts = ConstantCost 2,
      substitutionCosts = ConstantCost 10,
      transpositionCosts = ConstantCost 1
    }

-- | @cardToLink@ takes a Netrunner card and generates a link to its NetrunnerDB
-- page.
cardToLink :: Card -> Text
cardToLink card = case Card.code card of
  Nothing -> ""
  Just code -> pack $ "https://netrunnerdb.com/en/card/" ++ unpack code

-- | @cardToImage@ takes a Netrunner card and generates an embed image of the
-- card.
cardToImage :: NrApi -> Card -> Maybe CreateEmbedImage
cardToImage api card = do
  code <- Card.code card
  return $ CreateEmbedImageUrl $ replace "{code}" code $ imageTemplate api

-- | @cardToSubtitle@ generates the first line of a card's embed text listing
-- its types, subtypes, and various other data points.
cardToSubtitle :: NrApi -> Card -> Text
cardToSubtitle api card = pack $
  "**" ++
  type_code ++
  keywords ++
  cost ++
  strength ++
  agendaStats ++
  trash ++
  influence ++
  deckbuilding ++
  "**\n"
    where
      capitalise :: String -> String
      capitalise (x:xs) = toUpper x : xs
      type_code :: String
      type_code = capitalise $ unpack $ (fromMaybe "?") $ Card.type_code card
      keywords :: String
      keywords = case Card.keywords card of
        Nothing -> ""
        Just t -> ": " ++ unpack t
      cost :: String
      cost =
        let rezText = " • Rez: "
        in case (Card.cost card, Card.type_code card) of
        (Nothing, _) -> ""
        (Just x, Just "asset") -> rezText ++ show x
        (Just x, Just "ice") -> rezText ++ show x
        (Just x, Just "upgrade") -> rezText ++ show x
        (Just x, _) -> " • Cost: " ++ show x
      strength :: String
      strength = case Card.strength card of
        Nothing -> ""
        Just x -> " • Strength: " ++ show x
      agendaStats :: String
      agendaStats =
        let adv = fromMaybe "?" $ show <$> Card.advancement_cost card
            points = fromMaybe "?" $ show <$> Card.agenda_points card
        in case Card.type_code card of
          Just "agenda" -> " • " ++ adv ++ "/" ++ points
          _ -> ""
      trash :: String
      trash = case Card.trash_cost card of
        Nothing -> ""
        Just x -> " • Trash: " ++ show x
      influence :: String
      influence = case Card.faction_cost card of
        Nothing -> ""
        Just x ->
          if x == 0 && (fromMaybe "" $ Card.type_code card) `elem` ["agenda", "identity"]
          then ""
          else " • Influence: " ++ show x
      deckbuilding :: String
      deckbuilding = case Card.type_code card of
        Just "identity" -> " • " ++ ((fromMaybe "?") $ show <$> Card.minimum_deck_size card) ++ "/" ++ ((fromMaybe "?") $ show <$> Card.influence_limit card)
        Nothing -> ""
        _ -> ""

-- | @formatText@ takes a card's raw description and replaces the html
-- formatting tags with Discord formatting.
formatText :: Text -> Text
formatText raw =
  replace "<strong>" "**" $
  replace "</strong>" "**" $
  replace "<em>" "*" $
  replace "</em>" "*" $
  replace "<trace>" "**" $
  replace "</trace>" "**" $
  replace "[recurring-credit]" "⤼" $
  replace "[subroutine]" "↳" $
  replace "[credit]" "⬡" $
  replace "[click]" "∅" $
  replace "[trash]" "" $
  replace "[link]" "" $
  replace "[mu]" "μ" $
  raw

-- | @cardToFaction@ takes a card and attempts to find its faction.
cardToFaction :: NrApi -> Card -> Maybe Faction
cardToFaction api card = do
  faction <- Card.faction_code card
  let fRes = filter (\f -> Faction.code f == faction) $ factions api
  case fRes of
    [] -> Nothing
    (f:_) -> Just f

-- | @cardToPack@ takes a card and attempts to find its data pack.
cardToPack :: NrApi -> Card -> Maybe Pack
cardToPack api card = do
  cardPack <- Card.pack_code card
  let pRes = filter (\p -> Pack.code p == cardPack) $ packs api
  case pRes of
    [] -> Nothing
    (p:_) -> Just p

-- | @packToCycle@ takes a pack and attempts to find its cycle.
packToCycle :: NrApi -> Pack -> Maybe Cycle
packToCycle api pack =
  let cRes = filter (\c -> Cycle.code c == Pack.cycle_code pack) $ cycles api
  in case cRes of
    [] -> Nothing
    (c:_) -> Just c

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
      let faction = unpack $ Faction.name f
      let rotation = if Cycle.rotated c
                     then " (rotated)"
                     else ""
      let expansion = if Pack.name p == Cycle.name c
                      then unpack $ Pack.name p
                      else (unpack $ Cycle.name c) ++ rotation ++ " • " ++ (unpack $ Pack.name p)
      let position = fromMaybe "" $ (\t -> " #" ++ show t) <$> Card.position card -- Haskell can't parse this if I do it as a case of statment
      return $ pack $ faction ++ " • " ++ expansion ++ position

-- | @cardToColour@ gets the factional colour of a card to use in its embed.
cardToColour :: NrApi -> Card -> DiscordColour
cardToColour api card = fromMaybe Default helper
  where
    helper :: Maybe DiscordColour
    helper = do
      f <- cardToFaction api card
      return $ hexToDiscordColour $ unpack $ Faction.color f

-- | @cardToLink@ takes a Netrunner card and generates an embed message
-- representing it.
cardToEmbed :: NrApi -> Card -> Embed
cardToEmbed api card =
  let eTitle = fromMaybe "?" $ title card
      eURL = cardToLink card
      sub = unpack $ cardToSubtitle api card
      desc = unpack $ formatText $ fromMaybe "" $ text card
      eText = pack $ sub ++ desc
      eFoot = cardToReleaseData api card
      eImg = cardToImage api card
      eColour = cardToColour api card
  in addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg eText [] Nothing eFoot Nothing
