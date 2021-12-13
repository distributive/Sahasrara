-- |
-- Module      : Tablebot.Plugins.Netrunner
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Tablebot.Plugins.Netrunner (netrunnerPlugin) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, pack, unpack)
import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessage, sendEmbedMessage)
import Tablebot.Plugin.Exception (BotException (NetrunnerException), throwBot)
import Tablebot.Plugin.Netrunner
import Tablebot.Plugin.Netrunner.NrApi (NrApi, getNrApi)
import Tablebot.Plugin.Parser (netrunnerQuery, netrunnerCustom)
import Tablebot.Plugin.SmartCommand (PComm (parseComm), Quoted (Qu), RestOfInput1 (ROI1), WithError (WErr))
import Tablebot.Handler.Command ()
import Text.Megaparsec
import Text.RawString.QQ (r)
import Discord.Types

import Tablebot.Plugin.Netrunner.Custom (customCard)
import Debug.Trace

-- | @netrunner@ is the user-facing command that searches for Netrunner cards.
netrunner :: EnvCommand NrApi
netrunner =
  Command
    "netrunner"
    (parseComm nrComm)
    [nrFind, nrCustom]
      where
        nrComm ::
          WithError
            "Unknown Netrunner functionality"
            () ->
          Message ->
          EnvDatabaseDiscord NrApi ()
        nrComm _ m = sendMessage m "netrunner"

nrFind :: EnvCommand NrApi
nrFind = Command "find" (parseComm findComm) []
  where
    findComm ::
      WithError "No card title given!" (Either (Quoted Text) (RestOfInput1 Text)) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    findComm (WErr (Left (Qu q))) = searchCard q
    findComm (WErr (Right (ROI1 q))) = searchCard q

-- nrSearch :: EnvCommand NrApi
-- nrSearch = Command "search" (parseComm searchComm) []
--   where
--     searchComm ::
--

nrCustom :: EnvCommand NrApi
nrCustom = Command "custom" customPars []
  where
    customPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    customPars = do
      pairs <- netrunnerCustom
      return $ customFunc pairs
    customFunc :: [(String, String)] -> Message -> EnvDatabaseDiscord NrApi ()
    customFunc pairs m = do
      api <- ask
      let card = customCard api pairs
      sendEmbedMessage m "" $ cardToEmbed api card
      -- case customCard api pairs of
      --   Left err -> throwBot $ NetrunnerException err
      --   Right card -> sendEmbedMessage m "" $ cardToEmbed api card

-- | @nrFindInline@ is the inline version of @nrFind@.
nrFindInline :: EnvInlineCommand NrApi
nrFindInline = InlineCommand $ withRecovery (\pe -> trace (show pe) nrInlineComm) nrInlineComm
  where
    nrInlineComm :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    nrInlineComm = do
      queries <- many (try $ skipManyTill anySingle netrunnerQuery)
      return $ \m -> mapM_ (\q -> searchCard (pack q) m) queries

-- | @searchCard@ takes a query and outputs an embed representing the card whose
-- name best matches it.
searchCard :: Text -> Message -> EnvDatabaseDiscord NrApi ()
searchCard query m = do
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
netrunnerPlugin = (envPlug "netrunner" netrunnerStartUp) {commands = [netrunner], inlineCommands = [nrFindInline], helpPages = [netrunnerHelp]}
