-- |
-- Module      : Tablebot.Utility.Embed
-- Description : Embed manipulation helpers
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes some helpful functions for working with full Embeds
module Tablebot.Utility.Embed where

import Data.Text (Text)
import Discord.Internal.Types
import Tablebot.Internal.Embed (Embeddable, asEmbed, colourToInternal)
import Tablebot.Utility.Types (DiscordColour)

-- | Some helper functions to allow progressively building up an embed
-- If you need something more complex, you can still use the createEmbed flow provided by discord-haskell,
-- its not bad (once you realise that it turns empty strings into Nothing for you...) but it can't do colours.
-- To add a colour run createEmbed on it and then use one of these functions to manipulate it
simpleEmbed :: Text -> Embed
simpleEmbed t = createEmbed $ CreateEmbed "" "" Nothing "" "" Nothing t [] Nothing "" Nothing Nothing

addTitle :: Embeddable e => Text -> e -> Embed
addTitle t e =
  (asEmbed e)
    { embedTitle = Just t
    }

addFooter :: Embeddable e => Text -> e -> Embed
addFooter t e =
  (asEmbed e)
    { embedFooter = Just $ EmbedFooter t Nothing Nothing
    }

addTimestamp :: UTCTime -> Embed -> Embed
addTimestamp t e =
  e
    { embedTimestamp = Just t
    }

addAuthor :: Text -> Embed -> Embed
addAuthor t e =
  (asEmbed e)
    { embedAuthor = Just $ EmbedAuthor (Just t) Nothing Nothing Nothing
    }

addLink :: Text -> Embed -> Embed
addLink t e =
  e
    { embedUrl = Just t
    }

addColour :: DiscordColour -> Embed -> Embed
addColour c e =
  (asEmbed e)
    { embedColor = Just $ colourToInternal c
    }

addImage :: Embeddable e => Text -> e -> Embed
addImage url e =
  (asEmbed e)
    { embedImage = Just $ EmbedImage (Just url) Nothing Nothing Nothing
    }

addThumbnail :: Embeddable e => Text -> e -> Embed
addThumbnail url e =
  (asEmbed e)
    { embedThumbnail = Just $ EmbedThumbnail (Just url) Nothing Nothing Nothing
    }
