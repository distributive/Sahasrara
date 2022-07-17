{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The backend functionality of the Netrunner commands.
module Sahasrara.Plugins.ONR.Utility.Embed
  ( cardToEmbed,
    cardToEmbedWithText,
    cardToImgEmbed,
    cardToFlavourEmbed,
    embedText,
  )
where

import Data.Text (Text)
import Discord.Types
import Sahasrara.Plugins.ONR.Type.Card as Card (Card (..))
import Sahasrara.Plugins.ONR.Type.OnrApi (OnrApi (..))
import Sahasrara.Plugins.ONR.Utility.Card
import Sahasrara.Utility
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Types ()
import Prelude hiding (unwords)

-- | @cardToEmbed@ takes a card and generates an embed message representing it.
cardToEmbed :: OnrApi -> Card -> EnvDatabaseDiscord OnrApi CreateEmbed
cardToEmbed api card = do
  let eTitle = toTitle card
      eFoot = toReleaseData card
      eImg = toImage api card
      eColour = toColour card
  eText <- toText card
  return $ addColour eColour $ CreateEmbed "" "" Nothing eTitle "" eImg eText [] Nothing eFoot Nothing Nothing Nothing

-- | @cardToEmbedWithText@ embeds some text and decorates it with a given card.
cardToEmbedWithText :: OnrApi -> Card -> Text -> EnvDatabaseDiscord OnrApi CreateEmbed
cardToEmbedWithText api card text = do
  let eTitle = toTitle card
      eColour = toColour card
      eImg = toImage api card
  return $ addColour eColour $ CreateEmbed "" "" Nothing eTitle "" eImg text [] Nothing "" Nothing Nothing Nothing

-- | @cardToImgEmbed@ takes a card and attempts to embed a picture of it.
cardToImgEmbed :: OnrApi -> Card -> CreateEmbed
cardToImgEmbed api card =
  let eTitle = toTitle card
      eColour = toColour card
   in addColour eColour $ case toImage api card of
        Nothing -> CreateEmbed "" "" Nothing eTitle "" Nothing "`Could not find card art`" [] Nothing "" Nothing Nothing Nothing
        eImg -> CreateEmbed "" "" Nothing eTitle "" Nothing "" [] eImg "" Nothing Nothing Nothing

-- | @cardToFlavourEmbed@ takes a card and attempts to embed its flavour text.
cardToFlavourEmbed :: OnrApi -> Card -> EnvDatabaseDiscord OnrApi CreateEmbed
cardToFlavourEmbed api card = do
  let eTitle = toTitle card
      eColour = toColour card
      eImg = toImage api card
      fallback = CreateEmbed "" "" Nothing eTitle "" eImg "`Card has no flavour text`" [] Nothing "" Nothing Nothing Nothing
  flavour <- toFlavour card
  return $
    addColour eColour $ case flavour of
      Nothing -> fallback
      Just "" -> fallback
      Just eFlavour -> CreateEmbed "" "" Nothing eTitle "" eImg eFlavour [] Nothing "" Nothing Nothing Nothing

-- | @embedText@ just embeds the given text.
embedText :: Text -> Text -> CreateEmbed
embedText title text = CreateEmbed "" "" Nothing title "" Nothing text [] Nothing "" Nothing Nothing Nothing
