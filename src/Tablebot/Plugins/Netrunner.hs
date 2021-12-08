-- |
-- Module      : Tablebot.Plugins.Netrunner
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for generating welcome messages.
module Tablebot.Plugins.Netrunner (netrunnerPlugin) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, pack, unpack)
import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessage, sendEmbedMessage)
import Tablebot.Plugin.Exception (BotException (NetrunnerException), throwBot)
import Tablebot.Plugin.Netrunner
import Tablebot.Plugin.Netrunner.NrApi (NrApi, getNrApi)
import Tablebot.Plugin.Parser (netrunnerQuery)
import Tablebot.Plugin.SmartCommand (PComm (parseComm), Quoted (Qu), RestOfInput1 (ROI1) )
import Tablebot.Handler.Command ()
import Text.Megaparsec
import Text.RawString.QQ (r)
import Discord.Types

-- | @netrunner@ is the user-facing command that searches for Netrunner cards.
netrunner :: EnvCommand NrApi
netrunner = Command "netrunner" (parseComm nrComm) []
  where
    nrComm :: Maybe (Either (Quoted Text) (RestOfInput1 Text)) -> Message -> EnvDatabaseDiscord NrApi ()
    nrComm Nothing = \m -> sendMessage m "netrunner"
    nrComm (Just (Left (Qu q))) = nrFunc q
    nrComm (Just (Right (ROI1 q))) = nrFunc q

-- | @netrunnerInline@ is the inline version of @netrunner@.
netrunnerInline :: EnvInlineCommand NrApi
netrunnerInline = InlineCommand nrInlineComm
  where
    nrInlineComm :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    nrInlineComm =
      ( do
        queries <- many (try $ skipManyTill anySingle netrunnerQuery)
        return $ \m -> mapM_ (\q -> nrFunc (pack q) m) queries
      )

nrFunc :: Text -> Message -> EnvDatabaseDiscord NrApi ()
nrFunc query m = do
  api <- ask
  let res = queryCard api $ unpack query
  case res of
    Nothing -> throwBot $ NetrunnerException "No such card found!"
    Just card -> sendEmbedMessage m "" $ cardToEmbed api card

netrunnerHelp :: HelpPage
netrunnerHelp =
  HelpPage
    "netrunner"
    "netrunnernetrunnernetrunnernetrunnernetrunner"
    [r|**Netrunner**
netrunnernetrunnernetrunnernetrunnernetrunnernetrunner

*Usage:* `netrunner`|]
    []
    None

netrunnerStartUp :: StartUp NrApi
netrunnerStartUp = StartUp $ liftIO getNrApi

-- | @welcomePlugin@ assembles these commands into a plugin.
netrunnerPlugin :: EnvPlugin NrApi
netrunnerPlugin = (envPlug "netrunner" netrunnerStartUp) {commands = [netrunner], inlineCommands = [netrunnerInline], helpPages = [netrunnerHelp]}
