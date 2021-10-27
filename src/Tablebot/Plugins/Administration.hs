-- |
-- Module      : Tablebot.Plugins.Administration
-- Description : A command that outputs its input.
-- Copyright   : (c) Anna Bruce 2021
-- License     : MIT
-- Maintainer  : tablebot@ameliewd.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands that manage the loading and reloading of plugins
module Tablebot.Plugins.Administration (administrationPlugin) where

-- import from handler is unorthodox, but I don't want other plugins messing with that table...

import Control.Monad (when)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Database.Persist (Entity, Filter, entityVal, (==.))
import Discord (stopDiscord)
import Discord.Types
import Tablebot.Handler.Administration
import Tablebot.Handler.Types (CompiledPlugin (compiledName))
import Tablebot.Plugin
import Tablebot.Plugin.Database
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Permission (requirePermission)
import Tablebot.Plugin.SmartCommand
import Text.RawString.QQ

-- | @SS@ denotes the type returned by the command setup. Here its unused.
type SS = [Text]

blacklist :: Command SS
blacklist = Command "blacklist" (parseComm blacklistComm)

blacklistComm ::
  WithError
    "Unknown quote functionality."
    ( Either
        ( Either
            (Exactly "add", String)
            (Exactly "remove", String)
        )
        (Exactly "list")
    ) ->
  Message ->
  DatabaseDiscord SS ()
blacklistComm (WErr (Left (Left (_, pLabel)))) = addBlacklist pLabel
blacklistComm (WErr (Left (Right (_, pLabel)))) = removeBlacklist pLabel
blacklistComm (WErr (Right (_))) = listBlacklist

addBlacklist :: String -> Message -> DatabaseDiscord SS ()
addBlacklist pLabel m = requirePermission Superuser m $ do
  known <- ask
  -- It's not an error to add an unknown plugin (so that you can pre-disable a plugin you know you're about to add),
  -- but emmit a warning so people know if it wasn't deliberate
  when ((pack pLabel) `notElem` known) $ sendMessage m "Warning, unknown plugin"
  extant <- exists [PluginBlacklistLabel ==. pLabel]
  if not $ extant
    then do
      _ <- insert $ PluginBlacklist pLabel
      sendMessage m "Plugin added to blacklist. Please reload for it to take effect"
    else sendMessage m "Plugin already in blacklist"

removeBlacklist :: String -> Message -> DatabaseDiscord SS ()
removeBlacklist pLabel m = requirePermission Superuser m $ do
  extant <- selectKeysList [PluginBlacklistLabel ==. pLabel] []
  if not $ null extant
    then do
      _ <- delete (head extant)
      sendMessage m "Plugin removed from blacklist. Please reload for it to take effect"
    else sendMessage m "Plugin not in blacklist"

-- | @listBlacklist@ shows a list of the plugins eligible for disablement (those not starting with _),
--  along with their current status.
listBlacklist :: Message -> DatabaseDiscord SS ()
listBlacklist m = requirePermission Superuser m $ do
  bl <- selectList allBlacklisted []
  pl <- ask
  sendMessage m (format pl (blacklisted bl))
  where
    allBlacklisted :: [Filter PluginBlacklist]
    allBlacklisted = []
    -- Oh gods, so much formatting.
    format :: [Text] -> [Text] -> Text
    format p bl = "**Plugins:**\n```\n" <> (T.concat $ map (format' len' bl) (filter (disableable . T.uncons) p)) <> "```" <> unknown (filter (`notElem` p) bl)
      where
        len' :: Int
        len' = maximum $ map T.length p
    blacklisted :: [Entity PluginBlacklist] -> [Text]
    blacklisted pbl = map (pack . pluginBlacklistLabel . entityVal) pbl
    disableable :: Maybe (Char, Text) -> Bool
    disableable Nothing = False
    disableable (Just ('_', _)) = False
    disableable _ = True
    format' :: Int -> [Text] -> Text -> Text
    format' width bl a = (T.justifyLeft width ' ' a) <> " : " <> (if a `elem` bl then "DISABLED" else "ENABLED") <> "\n"
    unknown :: [Text] -> Text
    unknown [] = ""
    unknown l = "**Unknown blacklisted plugins**\n```\n" <> (T.concat $ map (<> ("\n" :: Text)) l) <> "```"

-- | @restart@ reloads the bot with any new configuration changes.
reload :: Command SS
reload = Command "reload" restartCommand
  where
    restartCommand :: Parser (Message -> DatabaseDiscord SS ())
    restartCommand = noArguments $ \m -> requirePermission Superuser m $ do
      sendMessage m "Reloading bot..."
      liftDiscord $ stopDiscord

reloadHelp :: HelpPage
reloadHelp =
  HelpPage
    "reload"
    "reload the bot"
    [r|**Restart**
Restart the bot

*Usage:* `reload`|]
    []
    Superuser

blacklistAddHelp :: HelpPage
blacklistAddHelp =
  HelpPage
    "add"
    "Disable a plugin"
    "**Blacklist Add**\n\
    \Disable a plugin. This does **not** check that the entered plugin is currently avaliable. \
    \This allows you to upgrade without having a new plugin enabled breifly.\n\n\
    \*Usage*: `blacklist add <plugin>`"
    []
    Superuser

blacklistRemoveHelp :: HelpPage
blacklistRemoveHelp =
  HelpPage
    "remove"
    "Enable a plugin"
    [r|**Blacklist Remove**
Re-enable a plugin.

*Usage*: `blacklist remove <plugin>`
|]
    []
    Superuser

blacklistListHelp :: HelpPage
blacklistListHelp =
  HelpPage
    "list"
    "List disabled plugins"
    [r|**Blacklist List**
List the current plugins in the blacklist.

*Usage*: `blacklist list`
|]
    []
    Superuser

blacklistHelp :: HelpPage
blacklistHelp =
  HelpPage
    "blacklist"
    "Enable and disable plugins"
    [r|**Blacklist**
Enable and disable plugins|]
    [blacklistListHelp, blacklistAddHelp, blacklistRemoveHelp]
    Superuser

adminStartup :: [CompiledPlugin] -> StartUp SS
adminStartup cps =
  StartUp
    (return $ map compiledName cps)

-- | @administrationPlugin@ assembles the commands into a plugin.
-- Note the use of an underscore in the name, this prevents the plugin being disabled.
administrationPlugin :: [CompiledPlugin] -> Plugin SS
administrationPlugin cps = (startPlug "_admin" $ adminStartup cps) {commands = [reload, blacklist], helpPages = [reloadHelp, blacklistHelp]}
