-- |
-- Module      : Tablebot.Plugins.Administration
-- Description : A set of commands that allows an administrator to manage the whole bot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands that manage the loading and reloading of plugins
module Tablebot.Plugins.Administration (administrationPlugin) where

-- import from internal is unorthodox, but I don't want other plugins messing with that table...

import Control.Concurrent.MVar (MVar, swapMVar)
import Control.Monad (when)
import Control.Monad.Cont (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Version (showVersion)
import Database.Persist (Entity, Filter, entityVal, (==.))
import Discord (stopDiscord)
import Discord.Types
import Language.Haskell.Printf (s)
import Tablebot.Internal.Administration
import Tablebot.Internal.Cache (getVersionInfo)
import Tablebot.Internal.Types (CompiledPlugin (compiledName))
import Tablebot.Utility
import Tablebot.Utility.Database
import Tablebot.Utility.Discord (sendMessage)
import Tablebot.Utility.Permission (requirePermission)
import Tablebot.Utility.SmartParser
import Text.RawString.QQ

-- | @SS@ denotes the type returned by the command setup. Here its unused.
type SS = [Text]

blacklist :: EnvCommand SS
blacklist = Command "blacklist" (parseComm blacklistComm) []

blacklistComm ::
  WithError
    "Unknown blacklist functionality."
    ( Either
        ( Either
            (Exactly "add", String)
            (Exactly "remove", String)
        )
        (Exactly "list")
    ) ->
  Message ->
  EnvDatabaseDiscord SS ()
blacklistComm (WErr (Left (Left (_, pLabel)))) = addBlacklist pLabel
blacklistComm (WErr (Left (Right (_, pLabel)))) = removeBlacklist pLabel
blacklistComm (WErr (Right (_))) = listBlacklist

addBlacklist :: String -> Message -> EnvDatabaseDiscord SS ()
addBlacklist pLabel m = requirePermission Superuser m $ do
  known <- ask
  -- It's not an error to add an unknown plugin (so that you can pre-disable a plugin you know you're about to add),
  -- but emmit a warning so people know if it wasn't deliberate
  when ((pack pLabel) `notElem` known) $ sendMessage m "Warning, unknown plugin"
  extant <- exists [PluginBlacklistLabel ==. pLabel]
  if not extant
    then do
      _ <- insert $ PluginBlacklist pLabel
      sendMessage m "Plugin added to blacklist. Please reload for it to take effect"
    else sendMessage m "Plugin already in blacklist"

removeBlacklist :: String -> Message -> EnvDatabaseDiscord SS ()
removeBlacklist pLabel m = requirePermission Superuser m $ do
  extant <- selectKeysList [PluginBlacklistLabel ==. pLabel] []
  if not $ null extant
    then do
      _ <- delete (head extant)
      sendMessage m "Plugin removed from blacklist. Please reload for it to take effect"
    else sendMessage m "Plugin not in blacklist"

-- | @listBlacklist@ shows a list of the plugins eligible for disablement (those not starting with _),
--  along with their current status.
listBlacklist :: Message -> EnvDatabaseDiscord SS ()
listBlacklist m = requirePermission Superuser m $ do
  bl <- selectList allBlacklisted []
  pl <- ask
  sendMessage m (format pl (blacklisted bl))
  where
    allBlacklisted :: [Filter PluginBlacklist]
    allBlacklisted = []
    -- Oh gods, so much formatting.
    format :: [Text] -> [Text] -> Text
    format p bl =
      pack $
        [s|**Plugins**
```
%Q
```
%Q|]
          (T.concat $ map (formatPluginState len' bl) (filter (disableable . T.uncons) p))
          (formatUnknownDisabledPlugins (filter (`notElem` p) bl))
      where
        len' :: Int
        len' = maximum $ map T.length p
    blacklisted :: [Entity PluginBlacklist] -> [Text]
    blacklisted pbl = map (pack . pluginBlacklistLabel . entityVal) pbl
    disableable :: Maybe (Char, Text) -> Bool
    disableable Nothing = False
    disableable (Just ('_', _)) = False
    disableable _ = True
    formatPluginState :: Int -> [Text] -> Text -> Text
    formatPluginState width bl a =
      pack $
        [s|%Q : %Q
|]
          (T.justifyLeft width ' ' a)
          (if a `elem` bl then "DISABLED" else "ENABLED")
    formatUnknownDisabledPlugins :: [Text] -> Text
    formatUnknownDisabledPlugins [] = ""
    formatUnknownDisabledPlugins l =
      pack $
        [s|**Unknown blacklisted plugins**
```
%Q
```|]
          (T.concat $ map (<> ("\n" :: Text)) l)

-- | @version@ identifies the .
version :: EnvCommand SS
version = Command "version" noCommand []
  where
    noCommand :: Parser (Message -> EnvDatabaseDiscord SS ())
    noCommand = noArguments $ \m -> do
      gVersion <- getVersionInfo
      sendMessage m $ formatVersions gVersion
    formatVersions :: VersionInfo -> Text
    formatVersions vi = "Tablebot version " <> (pack $ showVersion $ procVersion vi) <> "\nGit Hash: `" <> (gitHash vi) <> "`"

-- | @botcontrol@ reloads the bot with any new configuration changes.
botControl :: MVar ShutdownReason -> EnvCommand SS
botControl rFlag = Command "botcontrol" noCommand [reload rFlag, restart rFlag, halt rFlag, gitprompt rFlag]
  where
    noCommand :: Parser (Message -> EnvDatabaseDiscord SS ())
    noCommand = noArguments $ \m -> requirePermission Superuser m $ do
      sendMessage m "Please enter a subcommand"

-- | @reload@ reloads the bot with any new configuration changes.
reload :: MVar ShutdownReason -> EnvCommand SS
reload rFlag = Command "reload" restartCommand []
  where
    restartCommand :: Parser (Message -> EnvDatabaseDiscord SS ())
    restartCommand = noArguments $ \m -> requirePermission Superuser m $ do
      sendMessage m "Reloading bot..."
      _ <- liftIO $ swapMVar rFlag Reload
      liftDiscord $ stopDiscord

-- | @reload@ reloads the bot with any new configuration changes.
restart :: MVar ShutdownReason -> EnvCommand SS
restart rFlag = Command "restart" restartCommand []
  where
    restartCommand :: Parser (Message -> EnvDatabaseDiscord SS ())
    restartCommand = noArguments $ \m -> requirePermission Superuser m $ do
      sendMessage m "Restarting bot... (this may take some time)"
      _ <- liftIO $ swapMVar rFlag Restart
      liftDiscord $ stopDiscord

-- | @halt@ stops the bot.
halt :: MVar ShutdownReason -> EnvCommand SS
halt rFlag = Command "halt" restartCommand []
  where
    restartCommand :: Parser (Message -> EnvDatabaseDiscord SS ())
    restartCommand = noArguments $ \m -> requirePermission Superuser m $ do
      sendMessage m "Halting bot! (Goodnight, cruel world)"
      _ <- liftIO $ swapMVar rFlag Halt
      liftDiscord $ stopDiscord

-- | @gitupdate@ pulls the latest version from the git.
gitprompt :: MVar ShutdownReason -> EnvCommand SS
gitprompt rFlag = Command "gitupdate" promptCommand [gitupdate rFlag]
  where
    promptCommand :: Parser (Message -> EnvDatabaseDiscord SS ())
    promptCommand = noArguments $ \m -> requirePermission Superuser m $ do
      sendMessage m "Please confirm you want to do this by appending the following to your command:\n`yes I'm sure I want to do this and understand it's potentially dangerous`"

gitupdate :: MVar ShutdownReason -> EnvCommand SS
gitupdate rFlag = Command "yes I'm sure I want to do this and understand it's potentially dangerous" restartCommand []
  where
    restartCommand :: Parser (Message -> EnvDatabaseDiscord SS ())
    restartCommand = noArguments $ \m -> requirePermission Superuser m $ do
      sendMessage m "Attempting to update bot from the git. Please wait"
      _ <- liftIO $ swapMVar rFlag GitUpdate
      liftDiscord $ stopDiscord

versionHelp :: HelpPage
versionHelp =
  HelpPage
    "version"
    []
    "print version information"
    [r|**Version**
Print the current bot version and git hash of the running bot.

*Usage:* `version`|]
    []
    Superuser

botControlHelp :: HelpPage
botControlHelp =
  HelpPage
    "botcontrol"
    []
    "administrative commands"
    [r|**Bot Control**
General management commands for superuser use|]
    [reloadHelp, restartHelp, haltHelp, gitupdateHelp]
    Superuser

reloadHelp :: HelpPage
reloadHelp =
  HelpPage
    "reload"
    []
    "reload the bot"
    [r|**Reload**
Restart the bot without recompiling

*Usage:* `botcontrol reload`|]
    []
    Superuser

restartHelp :: HelpPage
restartHelp =
  HelpPage
    "restart"
    []
    "recompile and restart the bot"
    [r|**Restart**
Recompile and restart the bot

*Usage:* `botcontrol restart`|]
    []
    Superuser

haltHelp :: HelpPage
haltHelp =
  HelpPage
    "halt"
    []
    "stop the bot"
    [r|**Halt**
Stop the bot

*Usage:* `botcontrol halt`|]
    []
    Superuser

gitupdateHelp :: HelpPage
gitupdateHelp =
  HelpPage
    "gitupdate"
    []
    "use git to update the bot"
    [r|**Halt**
Update the bot from git
Will attempt to pull the latest version from origin.
Requires that the working state is clean, and that it can be merged with the incoming without conflict.

Requires `ALLOW_GIT_UPDATE` to be true. 

*Usage:* `botcontrol gitupdate`|]
    []
    Superuser

blacklistAddHelp :: HelpPage
blacklistAddHelp =
  HelpPage
    "add"
    []
    "disable a plugin"
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
    []
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
    []
    "list disabled plugins"
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
    []
    "enable and disable plugins"
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
administrationPlugin :: MVar ShutdownReason -> [CompiledPlugin] -> EnvPlugin SS
administrationPlugin rFlag cps = (envPlug "_admin" $ adminStartup cps) {commands = [botControl rFlag, blacklist, version], helpPages = [versionHelp, botControlHelp, blacklistHelp]}
