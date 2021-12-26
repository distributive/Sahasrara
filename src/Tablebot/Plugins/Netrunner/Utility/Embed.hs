{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Tablebot.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The backend functionality of the Netrunner commands.
module Tablebot.Plugins.Netrunner.Utility.Embed
  ( cardToEmbed,
    cardsToEmbed,
    cardToImgEmbed,
    cardToFlavourEmbed,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, pack)
import Discord.Types
import Tablebot.Plugins.Netrunner.Type.Card as Card (Card (..))
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Utility.Card
import Tablebot.Plugins.Netrunner.Utility.Faction
import Tablebot.Utility
import Tablebot.Utility.Embed (addColour)
import Tablebot.Utility.Types ()
import Prelude hiding (unwords)

-- | @cardToEmbed@ takes a card and generates an embed message representing it.
cardToEmbed :: NrApi -> Card -> EnvDatabaseDiscord NrApi Embed
cardToEmbed api card = do
  let eTitle = toTitle card
      eURL = toLink card
      eFoot = toReleaseData api card
      eImg = toImage api card
      eColour = toColour api card
  eText <- toText card
  return $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg eText [] Nothing eFoot Nothing Nothing

-- | @cardsToEmbed@ takes a list of cards and embeds their names with links.
cardsToEmbed :: NrApi -> Text -> [Card] -> Text -> EnvDatabaseDiscord NrApi Embed
cardsToEmbed api pre cards err = do
  formatted <- mapM formatCard $ take 10 cards
  let cards' = "**" <> intercalate "\n" formatted <> "**"
      eTitle = "**" <> pack (show $ length cards) <> " results**"
      eText = pre <> "\n" <> cards' <> if length cards > 10 then "\n" <> err else ""
  return $ createEmbed $ CreateEmbed "" "" Nothing eTitle "" Nothing eText [] Nothing "" Nothing Nothing
  where
    formatCard :: Card -> EnvDatabaseDiscord NrApi Text
    formatCard card = do
      let title' = fromMaybe "?" $ title card
          link = toLink card
      icon <- case toFaction api card of
        Nothing -> return ""
        Just faction -> toEmoji faction
      return $ icon <> " [" <> title' <> "](" <> link <> ")"

-- | @cardToImgEmbed@ takes a card and attempts to embed a picture of it.
cardToImgEmbed :: NrApi -> Card -> Maybe Embed
cardToImgEmbed api card =
  let eTitle = toTitle card
      eURL = toLink card
      eColour = toColour api card
   in case toImage api card of
        Nothing -> Nothing
        eImg -> Just $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL Nothing "" [] eImg "" Nothing Nothing

-- | @cardToFlavourEmbed@ takes a card and attempts to embed its flavour text.
cardToFlavourEmbed :: NrApi -> Card -> EnvDatabaseDiscord NrApi (Maybe Embed)
cardToFlavourEmbed api card = do
  let eTitle = toTitle card
      eURL = toLink card
      eColour = toColour api card
      eImg = toImage api card
  flavor <- toFlavour card
  return $ case flavor of
    Nothing -> Nothing
    Just "" -> Nothing
    Just eFlavour -> Just $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg eFlavour [] Nothing "" Nothing Nothing
