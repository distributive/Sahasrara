-- |
-- Module      : Sahasrara.Plugins.Netrunner.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cards in Sahasrara.
module Sahasrara.Plugins.ONR.Utility.Card
  ( toTitle,
    toText,
    toImage,
    toSubtitle,
    toReleaseData,
    toColour,
    toFlavour,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, replace)
import qualified Data.Text (toTitle)
import Discord.Types
import Sahasrara.Plugins.ONR.Type.Card (Card (..), Stat (..))
import Sahasrara.Plugins.ONR.Type.OnrApi (OnrApi (..))
import Sahasrara.Plugins.ONR.Utility.Misc (formatNr)
import Sahasrara.Utility
import Sahasrara.Utility.Types ()

-- | @toImage@ takes a Netrunner card and loads an embed image of it.
toImage :: OnrApi -> Card -> Maybe CreateEmbedImage
toImage api card = do
  code' <- code card
  return $ CreateEmbedImageUrl $ replace "{code}" code' $ imageTemplate api

-- | @toTitle@ takes a Netrunner card and attempts to get its title.
toTitle :: Card -> Text
toTitle card = fromMaybe "?" $ title card

-- | @toText@ takes a Netrunner card, collects its data and textbox into a
-- single string.
toText :: Card -> EnvDatabaseDiscord OnrApi Text
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
    <> subtypes'
    <> cost'
    <> mu
    <> strength'
    <> agendaStats
    <> trash
    <> "**\n"
  where
    maybeIntToText = maybe "?" intToText
    maybeEmptyPrependI s mi = maybeEmptyPrepend s (intToText <$> mi)
    typeCode' = maybe "?" Data.Text.toTitle typeCode
    subtypes' = case subtypes of
      Nothing -> ""
      Just [] -> ""
      Just ss -> ": " <> intercalate " - " ss
    cost' =
      let rezText = " • Rez: "
          c = case cost of
            Nothing -> ""
            Just (Var var) -> var
            Just (Val val) -> intToText val
       in case typeCode of
            Just "node" -> rezText <> c
            Just "ice" -> rezText <> c
            Just "upgrade" -> rezText <> c
            _ -> " • Cost: " <> c
    mu = maybeEmptyPrependI " • MU: " memoryCost
    strength' = case strength of
      Nothing -> ""
      Just x ->
        " • Strength: " <> case x of
          Var var -> var
          Val val -> intToText val
    agendaStats =
      let adv = maybeIntToText difficulty
          points = maybeIntToText agendaPoints
       in case typeCode of
            Just "agenda" -> " • " <> adv <> "/" <> points
            _ -> ""
    trash = maybeEmptyPrependI " • Trash: " trashCost

-- | @toReleaseData@ combines the set of a card with its rarity.
toReleaseData :: Card -> Text
toReleaseData card = fromMaybe "" $ do
  s <- set card
  r <- rarity card
  return $ s <> " • " <> r

-- | @toColour@ gets the factional colour of a card to use in its embed.
toColour :: Card -> DiscordColour
toColour card = case rarity card of
  Just "Uncommon" -> RGB 71 177 80
  Just "Rare" -> RGB 71 93 177
  Just "Vital" -> RGB 145 71 177
  _ -> RGB 177 150 71

-- | @toFlavour@ gets a cards flavour text.
toFlavour :: Card -> EnvDatabaseDiscord OnrApi (Maybe Text)
toFlavour Card {flavour = flavour} = case flavour of
  Nothing -> return Nothing
  Just f -> do
    f' <- formatNr f
    return $ Just f'
