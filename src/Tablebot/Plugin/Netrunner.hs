{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Tablebot.Plugin.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- ...
module Tablebot.Plugin.Netrunner (cardToEmbed, queryCard) where

import Data.Char (toLower, toUpper)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, replace)
import Discord.Types
import Tablebot.Plugin.Netrunner.NrApi (NrApi (..))
import Tablebot.Plugin.Netrunner.Card as Card
import Tablebot.Plugin.Netrunner.Cycle as Cycle
import Tablebot.Plugin.Netrunner.Faction as Faction
import Tablebot.Plugin.Netrunner.Pack as Pack
import Text.EditDistance

-- | @queryCard@ fuzzy searches the given library of cards by title.
queryCard :: NrApi -> String -> Maybe Card
queryCard api query = Just $ minimumBy comparison $ reverse $ cards api
  where
    comparison :: Card -> Card -> Ordering
    comparison a b
      | score a < score b = LT
      | score a > score b = GT
      | otherwise = EQ
    score card = levenshteinDistance editCosts (map toLower query) $ map toLower $ unpack $ Card.title card
    editCosts = EditCosts {
      deletionCosts = ConstantCost 10,
      insertionCosts = ConstantCost 2,
      substitutionCosts = ConstantCost 10,
      transpositionCosts = ConstantCost 1
    }

-- | @cardToLink@ takes a Netrunner card and generates a link to its NetrunnerDB
-- page.
cardToLink :: Card -> Text
cardToLink card = pack $ "https://netrunnerdb.com/en/card/" ++ (unpack $ Card.code card)

-- | @cardToImage@ takes a Netrunner card and generates an embed image of the
-- card.
cardToImage :: NrApi -> Card -> CreateEmbedImage
cardToImage api card = CreateEmbedImageUrl $ replace "{code}" (Card.code card) $ imageTemplate api

-- | @cardToSubtitle@ generates the first line of a card's embed text listing
-- its types, subtypes, and various other data points.
cardToSubtitle :: NrApi -> Card -> Text
cardToSubtitle api card = pack $
  "**" ++
  type_code card ++
  keywords card ++
  cost card ++
  strength card ++
  advancement card ++
  points card ++
  trash card ++
  influence card ++
  "**\n"
    where
      capitalise :: String -> String
      capitalise (x:xs) = toUpper x : xs
      type_code :: Card -> String
      type_code = capitalise . unpack . Card.type_code
      keywords :: Card -> String
      keywords card = case Card.keywords card of
        Nothing -> ""
        Just t -> ": " ++ unpack t
      cost :: Card -> String
      cost card =
        let rezText = " • Rez: "
        in case (Card.cost card, Card.type_code card) of
        (Nothing, _) -> ""
        (Just x, "asset") -> rezText ++ show x
        (Just x, "ice") -> rezText ++ show x
        (Just x, "upgrade") -> rezText ++ show x
        (Just x, _) -> " • Cost: " ++ show x
      strength :: Card -> String
      strength card = case Card.strength card of
        Nothing -> ""
        Just x -> " • Strength: " ++ show x
      advancement :: Card -> String
      advancement card = case Card.advancement_cost card of
        Nothing -> ""
        Just x -> " • Adv: " ++ show x
      points :: Card -> String
      points card = case Card.agenda_points card of
        Nothing -> ""
        Just x -> " • Score: " ++ show x
      trash :: Card -> String
      trash card = case Card.trash_cost card of
        Nothing -> ""
        Just x -> " • Trash: " ++ show x
      influence :: Card -> String
      influence card
        | Card.faction_cost card == 0 && Card.type_code card `elem` ["agenda", "identity"] = ""
        | otherwise = " • Influence: " ++ (show $ Card.faction_cost card)

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
cardToFaction api card =
  let fRes = filter (\f -> Faction.code f == Card.faction_code card) $ factions api
  in case fRes of
    [] -> Nothing
    (f:_) -> Just f

-- | @cardToPack@ takes a card and attempts to find its data pack.
cardToPack :: NrApi -> Card -> Maybe Pack
cardToPack api card =
  let pRes = filter (\p -> Pack.code p == Card.pack_code card) $ packs api
  in case pRes of
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
      let expansion = if Pack.name p == Cycle.name c
                      then unpack $ Pack.name p
                      else (unpack $ Cycle.name c) ++ " • " ++ (unpack $ Pack.name p)
      let position = show $ Card.position card
      let rotation = if Cycle.rotated c
                     then " (rotated)"
                     else ""
      return $ pack $ faction ++ " • " ++ expansion ++ rotation ++ " " ++ position

-- | @cardToLink@ takes a Netrunner card and generates an embed message
-- representing it.
cardToEmbed :: NrApi -> Card -> Embed
cardToEmbed api card =
  let eTitle = title card
      eURL = cardToLink card
      sub = unpack $ cardToSubtitle api card
      desc = unpack $ formatText $ fromMaybe "" $ text card
      eText = pack $ sub ++ desc
      eFoot = cardToReleaseData api card
      eImg = cardToImage api card
  in createEmbed $ CreateEmbed "" "" Nothing eTitle eURL (Just eImg) eText [] Nothing eFoot Nothing
    where
