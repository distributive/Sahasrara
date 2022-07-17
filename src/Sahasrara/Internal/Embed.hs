-- -- |
-- Module      : Sahasrara.Internal.Embed
-- Description : Embed request generation and colours
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains some behind the scenes logic to allow creation of coloured embeds
module Sahasrara.Internal.Embed where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Types (CreateEmbed (CreateEmbed))
import Network.HTTP.Client.MultipartFormData (partBS)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import Sahasrara.Utility.Types

class Embeddable e where
  asEmbed :: e -> CreateEmbed

instance Embeddable CreateEmbed where
  asEmbed = id

instance Embeddable Text where
  asEmbed t = CreateEmbed "" "" Nothing "" "" Nothing t [] Nothing "" Nothing Nothing Nothing
