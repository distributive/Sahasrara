-- -- |
-- Module      : Tablebot.Internal.Embed
-- Description : Embed request generation and colours
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains some behind the scenes logic to allow creation of coloured embeds
module Tablebot.Internal.Embed where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Network.HTTP.Client.MultipartFormData (partBS)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import Tablebot.Utility.Types

colourToInternal :: DiscordColour -> Integer
colourToInternal (RGB r g b) = ((r * 256) + g) * 256 + b
colourToInternal Default = 0
colourToInternal Aqua = 1752220
colourToInternal DarkAqua = 1146986
colourToInternal Green = 3066993
colourToInternal DarkGreen = 2067276
colourToInternal Blue = 3447003
colourToInternal DarkBlue = 2123412
colourToInternal Purple = 10181046
colourToInternal DarkPurple = 7419530
colourToInternal LuminousVividPink = 15277667
colourToInternal DarkVividPink = 11342935
colourToInternal Gold = 15844367
colourToInternal DarkGold = 12745742
colourToInternal Orange = 15105570
colourToInternal DarkOrange = 11027200
colourToInternal Red = 15158332
colourToInternal DarkRed = 10038562
colourToInternal Gray = 9807270
colourToInternal DarkGray = 9936031
colourToInternal DarkerGray = 8359053
colourToInternal LightGray = 12370112
colourToInternal Navy = 3426654
colourToInternal DarkNavy = 2899536
colourToInternal Yellow = 16776960
colourToInternal DiscordWhite = 16777215
colourToInternal DiscordBlurple = 5793266
colourToInternal DiscordGrayple = 10070709
colourToInternal DiscordDarkButNotBlack = 2895667
colourToInternal DiscordNotQuiteBlack = 2303786
colourToInternal DiscordGreen = 5763719
colourToInternal DiscordYellow = 16705372
colourToInternal DiscordFuschia = 15418782
colourToInternal DiscordRed = 15548997
colourToInternal DiscordBlack = 16777215

-- | TablebotEmbedRequest is a request object that mimics various bits of the discord api, just so we can add colours.
-- This is *really* janky. The library exposes *no way* to create a coloured embed through its main api,
-- so I'm having to manually reimplement the sending logic just to add this in.
-- If you suffer from nightmares, don't look in 'Tablebot.Handler.Embed'. Nothing good lives there.
-- In the future, I may actually submit a PR to discord-haskell with a fix to allow colours properly.
channels :: R.Url 'R.Https
channels = baseUrl /: "channels"

data TablebotEmbedRequest a where TablebotEmbedRequest :: ChannelId -> Text -> Embed -> TablebotEmbedRequest Message

instance Request (TablebotEmbedRequest a) where
  jsonRequest = createEmbedJson
  majorRoute = embedMajorRoute

embedMajorRoute :: TablebotEmbedRequest a -> String
embedMajorRoute (TablebotEmbedRequest chan _ _) = "msg " <> show chan

createEmbedJson :: TablebotEmbedRequest a -> JsonRequest
createEmbedJson (TablebotEmbedRequest chan msg embed) =
  let partJson = partBS "payload_json" $ BL.toStrict $ encode $ toJSON $ object ["content" .= msg, "embed" .= embed]
      body = R.reqBodyMultipart [partJson]
   in Post (channels // chan /: "messages") body mempty

class Embeddable e where
  asEmbed :: e -> Embed

instance Embeddable Embed where
  asEmbed = id

instance Embeddable CreateEmbed where
  asEmbed = createEmbed

instance Embeddable Text where
  asEmbed t = createEmbed $ CreateEmbed "" "" Nothing "" "" Nothing t [] Nothing "" Nothing Nothing
