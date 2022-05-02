-- |
-- Module      : Sahasrara.Plugins.ONR.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Sahasrara.Plugins.ONR.Plugin (onrPlugin) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Text (pack)
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.ONR.Type.Card (Card)
import Sahasrara.Plugins.ONR.Type.OnrApi (OnrApi (..))
import Sahasrara.Plugins.ONR.Utility.Embed
import Sahasrara.Plugins.ONR.Utility.Find
import Sahasrara.Plugins.ONR.Utility.OnrApi (getOnrApi)
import Sahasrara.Utility
import Sahasrara.Utility.Discord (sendEmbedMessage)
import Sahasrara.Utility.Parser (inlineCommandHelper)
import Sahasrara.Utility.Types ()
import Text.Megaparsec (anySingleBut, some)
import Text.RawString.QQ (r)

-- | @nrInline@ searches for cards by name.
nrInline :: EnvInlineCommand OnrApi
nrInline = inlineCommandHelper "[|" "|]" (some $ anySingleBut '|') $ \query m -> do
  api <- ask
  embedCard (queryCard api $ pack query) m

-- | @nrInlineImg@ searches for a card and outputs an image of it.
nrInlineImg :: EnvInlineCommand OnrApi
nrInlineImg = inlineCommandHelper "{|" "|}" (some $ anySingleBut '|') $ \query m -> do
  api <- ask
  embedCardImg (queryCard api $ pack query) m

-- | @nrInlineFlavour@ searches for a card and outputs its flavour.
nrInlineFlavour :: EnvInlineCommand OnrApi
nrInlineFlavour = inlineCommandHelper "<|" "|>" (some $ anySingleBut '|') $ \query m -> do
  api <- ask
  embedCardFlavour (queryCard api $ pack query) m

-- | @embedCard@ takes a card and embeds it in a message.
embedCard :: Card -> Message -> EnvDatabaseDiscord OnrApi ()
embedCard card m = do
  api <- ask
  sendEmbedMessage m "" =<< cardToEmbed api card

-- | @embedCardImg@ embeds a card's image in a message, if able.
embedCardImg :: Card -> Message -> EnvDatabaseDiscord OnrApi ()
embedCardImg card m = do
  api <- ask
  sendEmbedMessage m "" $ cardToImgEmbed api card

-- | @embedCardFlavour@ embeds a card's flavour in a message, if able.
embedCardFlavour :: Card -> Message -> EnvDatabaseDiscord OnrApi ()
embedCardFlavour card m = do
  api <- ask
  embed <- cardToFlavourEmbed api card
  sendEmbedMessage m "" embed

-- | @onrStartUp@ loads the ONR api once at start up
onrStartUp :: StartUp OnrApi
onrStartUp = StartUp $ liftIO getOnrApi

-- | @onrPlugin@ assembles these commands into a plugin.
onrPlugin :: EnvPlugin OnrApi
onrPlugin =
  (envPlug "onr" onrStartUp)
    { commands = [],
      inlineCommands = [nrInline, nrInlineImg, nrInlineFlavour],
      helpPages = []
    }
