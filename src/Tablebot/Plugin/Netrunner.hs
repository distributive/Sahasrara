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

import Data.Aeson (FromJSON, Value (Object), (.:), eitherDecode, parseJSON)
import Data.Char (toLower)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, replace)
import Discord.Types
import Tablebot.Plugin.Netrunner.NrApi (NrApi (..), getNrApi)
import Tablebot.Plugin.Netrunner.Card as Card
import Tablebot.Plugin.Netrunner.Cycle as Cycle
import Tablebot.Plugin.Netrunner.Pack as Pack
import Text.EditDistance

-- | @queryCard@ fuzzy searches the given library of cards by title.
queryCard :: NrApi -> String -> Maybe Card
queryCard api query = Just $ minimumBy comparison $ cards api
  where
    comparison :: Card -> Card -> Ordering
    comparison a b
      | score a < score b = LT
      | score a > score b = GT
      | otherwise = EQ
    score card = levenshteinDistance editCosts (map toLower query) $ map toLower $ unpack $ Card.title card
    editCosts = EditCosts {
      deletionCosts = ConstantCost 10,
      insertionCosts = ConstantCost 1,
      substitutionCosts = ConstantCost 10,
      transpositionCosts = ConstantCost 5
    }

-- | @cardToLink@ takes a Netrunner card and generates a link to its NetrunnerDB
-- page.
cardToLink :: Card -> Text
cardToLink card = pack $ "https://netrunnerdb.com/en/card/" ++ (unpack $ Card.code card)

-- | @cardToImage@ takes a Netrunner card and generates an embed image of the
-- card.
cardToImage :: NrApi -> Card -> CreateEmbedImage
cardToImage api card = CreateEmbedImageUrl $ replace "{code}" (Card.code card) $ imageTemplate api

-- | @formatText@ takes a card's raw description and replaces the html
-- formatting tags with Discord formatting.
formatText :: Text -> Text
formatText raw =
  replace "<strong>" "**" $
  replace "</strong>" "**" $
    replace "<trace>" "**" $
    replace "</trace>" "**" $
  replace "[subroutine]" "↳" $
  replace "[credit]" "⬡" $
  replace "[click]" "∅" $
  replace "[recurring-credit]" "" $
  replace "[trash]" "" $
  raw

-- | @cardToLink@ takes a Netrunner card and generates an embed message
-- representing it.
cardToEmbed :: NrApi -> Card -> Embed
cardToEmbed api card =
  let eTitle = title card
      eURL = cardToLink card
      eDesc = formatText $ fromMaybe "" $ text card
      eFoot = pack $ (unpack $ faction_code card) ++ " • " ++ (unpack $ pack_code card) ++ " " ++ (show $ Card.position card)
      eImg = cardToImage api card
  in createEmbed $ CreateEmbed "" "" Nothing eTitle eURL (Just eImg) eDesc [] Nothing eFoot Nothing
