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
    cardToEmbedWithText,
    cardsToEmbed,
    cardToImgEmbed,
    cardToFlavourEmbed,
    embedText,
    embedTextWithUrl,
    embedColumns,
    embedLines,
  )
where

import Data.List.Split (splitPlaces)
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T (length)
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

-- | @cardToEmbedWithText@ embeds some text and decorates it with a given card.
cardToEmbedWithText :: NrApi -> Card -> Text -> EnvDatabaseDiscord NrApi Embed
cardToEmbedWithText api card text = do
  let eTitle = toTitle card
      eURL = toLink card
      eColour = toColour api card
      eImg = toImage api card
  return $ addColour eColour $ createEmbed $ CreateEmbed "" "" Nothing eTitle eURL eImg text [] Nothing "" Nothing Nothing

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
cardToImgEmbed :: NrApi -> Card -> Embed
cardToImgEmbed api card =
  let eTitle = toTitle card
      eURL = toLink card
      eColour = toColour api card
   in addColour eColour $
        createEmbed $ case toImage api card of
          Nothing -> CreateEmbed "" "" Nothing eTitle eURL Nothing "`Could not find card art`" [] Nothing "" Nothing Nothing
          eImg -> CreateEmbed "" "" Nothing eTitle eURL Nothing "" [] eImg "" Nothing Nothing

-- | @cardToFlavourEmbed@ takes a card and attempts to embed its flavour text.
cardToFlavourEmbed :: NrApi -> Card -> EnvDatabaseDiscord NrApi Embed
cardToFlavourEmbed api card = do
  let eTitle = toTitle card
      eURL = toLink card
      eColour = toColour api card
      eImg = toImage api card
      fallback = CreateEmbed "" "" Nothing eTitle eURL eImg "`Card has no flavour text`" [] Nothing "" Nothing Nothing
  flavor <- toFlavour card
  return $
    addColour eColour $
      createEmbed $ case flavor of
        Nothing -> fallback
        Just "" -> fallback
        Just eFlavour -> CreateEmbed "" "" Nothing eTitle eURL eImg eFlavour [] Nothing "" Nothing Nothing

-- | @embedText@ just embeds the given text.
embedText :: Text -> Text -> Embed
embedText title text = createEmbed $ CreateEmbed "" "" Nothing title "" Nothing text [] Nothing "" Nothing Nothing

-- | @embedTextWithUrl@ is @embedText@ but you can set the title URL.
embedTextWithUrl :: Text -> Text -> Text -> Embed
embedTextWithUrl title url text = createEmbed $ CreateEmbed "" "" Nothing title url Nothing text [] Nothing "" Nothing Nothing

-- | @embedColumns@ embeds Text into columns.
embedColumns :: Text -> Text -> [(Text, [Text])] -> Embed
embedColumns title pre cols =
  let fields = map (\x -> EmbedField (fst x) (intercalate "\n" $ snd x) $ Just True) cols
   in createEmbed $ CreateEmbed "" "" Nothing title "" Nothing pre fields Nothing "" Nothing Nothing

-- | @embedLines@ embeds a list of lines, splitting them into columns as needed.
-- NOTE: does not preserve order
embedLines :: Text -> Text -> [Text] -> Embed
embedLines title pre xs =
  let cumLength = scanl (\l x -> 1 + T.length x + l) (T.length title + 2) xs -- +1 for each newline title characters
      safeIndex = length $ takeWhile (< 1900) cumLength -- 1900 instead of 2000 because I gave up trying to be exact
      xs' = take safeIndex xs
      c = if length xs' < 12 then 1 else 2 --if length xs' < 27 then 2 else 3
      d = length xs' `div` c
      m = length xs' `mod` c
      heights = replicate m (d + 1) ++ replicate (c - m) d
      cols = splitPlaces heights xs'
      fields = map (\x -> EmbedField "⠀" (intercalate "\n" x) $ Just True) cols
   in createEmbed $ CreateEmbed "" "" Nothing title "" Nothing pre fields Nothing "" Nothing Nothing
