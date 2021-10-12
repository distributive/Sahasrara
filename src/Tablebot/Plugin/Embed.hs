-- -- |
-- Module      : Tablebot.Plugin.Embed
-- Description : Embed manipulation helpers
-- Copyright   : (c) Anna Bruce 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exposes some helpful functions for working with full Embeds
module Tablebot.Plugin.Embed where

import Data.Text (Text)
import Discord.Internal.Types
import Tablebot.Handler.Embed (colourToInternal)
import Tablebot.Plugin.Types (DiscordColour)

-- | Some helper functions to allow progressively building up an embed
-- If you need something more complex, you can still use the createEmbed flow provided by discord-haskell,
-- its not bad (once you realise that it turns empty strings into Nothing for you...) but it can't do colours.
-- To add a colour run createEmbed on it and then use one of these functions to manipulate it
simpleEmbed :: Text -> Embed
simpleEmbed t = createEmbed $ CreateEmbed "" "" Nothing "" "" Nothing t [] Nothing "" Nothing

addTitle :: Text -> Embed -> Embed
addTitle t e =
  e
    { embedTitle = Just t
    }

addFooter :: Text -> Embed -> Embed
addFooter t e =
  e
    { embedFooter = Just $ EmbedFooter t Nothing Nothing
    }

addTimestamp :: UTCTime -> Embed -> Embed
addTimestamp t e =
  e
    { embedTimestamp = Just t
    }

addAuthor :: Text -> Embed -> Embed
addAuthor t e =
  e
    { embedAuthor = Just $ EmbedAuthor (Just t) Nothing Nothing Nothing
    }

addLink :: Text -> Embed -> Embed
addLink t e =
  e
    { embedUrl = Just t
    }

addColour :: DiscordColour -> Embed -> Embed
addColour c e =
  e
    { embedColor = Just $ colourToInternal c
    }

addImage :: Text -> Embed -> Embed
addImage url e =
  e
    { embedImage = Just $ EmbedImage (Just url) Nothing Nothing Nothing
    }

addThumbnail :: Text -> Embed -> Embed
addThumbnail url e =
  e
    { embedThumbnail = Just $ EmbedThumbnail (Just url) Nothing Nothing Nothing
    }
