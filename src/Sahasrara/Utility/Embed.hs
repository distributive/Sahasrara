-- |
-- Module      : Sahasrara.Utility.Embed
-- Description : Embed manipulation helpers
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes some helpful functions for working with full Embeds
module Sahasrara.Utility.Embed where

import Data.Text (Text)
import Discord.Internal.Types
import Sahasrara.Internal.Embed (Embeddable, asEmbed)

-- | Some helper functions to allow progressively building up an embed
-- If you need something more complex, you can still use the createEmbed flow provided by discord-haskell,
-- its not bad (once you realise that it turns empty strings into Nothing for you...) but it can't do colours.
-- To add a colour run createEmbed on it and then use one of these functions to manipulate it
simpleEmbed :: Text -> CreateEmbed
simpleEmbed t = CreateEmbed "" "" Nothing "" "" Nothing t [] Nothing "" Nothing Nothing Nothing

basicEmbed :: Text -> Text -> CreateEmbed
basicEmbed title body = CreateEmbed "" "" Nothing title "" Nothing body [] Nothing "" Nothing Nothing Nothing

addTitle :: Embeddable e => Text -> e -> CreateEmbed
addTitle t e =
  (asEmbed e)
    { createEmbedTitle = t
    }

addFooter :: Embeddable e => Text -> e -> CreateEmbed
addFooter t e =
  (asEmbed e)
    { createEmbedFooterText = t
    }

addTimestamp :: Embeddable e => UTCTime -> e -> CreateEmbed
addTimestamp t e =
  (asEmbed e)
    { createEmbedTimestamp = Just t
    }

addAuthor :: Embeddable e => Text -> e -> CreateEmbed
addAuthor t e =
  (asEmbed e)
    { createEmbedAuthorName = t
    }

addLink :: Embeddable e => Text -> e -> CreateEmbed
addLink t e =
  (asEmbed e)
    { createEmbedUrl = t
    }

addColour :: Embeddable e => DiscordColor -> e -> CreateEmbed
addColour c e =
  (asEmbed e)
    { createEmbedColor = Just c
    }

addImage :: Embeddable e => Text -> e -> CreateEmbed
addImage url e =
  (asEmbed e)
    { createEmbedImage = Just $ CreateEmbedImageUrl url
    }

addThumbnail :: Embeddable e => Text -> e -> CreateEmbed
addThumbnail url e =
  (asEmbed e)
    { createEmbedThumbnail = Just $ CreateEmbedImageUrl url
    }
