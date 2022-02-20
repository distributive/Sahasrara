-- |
-- Module      : Tablebot.Utility.Types
-- Description : Types used throughout plugins.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- All of the important types used throughout the implementation. Defines plugins,
-- which are made out of features. Also defines how to construct and combine
-- plugins, and the @DatabaseDiscord@ monad transformer stack for allowing
-- database and Discord operations within your features.
module Tablebot.Utility.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toLower)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Version.Extra (Version)
import Data.Void (Void)
import Database.Persist.Sqlite (Migration, SqlPersistM, SqlPersistT)
import Discord (DiscordHandler)
import Discord.Types
  ( ChannelId,
    Emoji,
    Event (..),
    Message,
    MessageId,
    ReactionInfo,
  )
import Safe.Exact (dropExactMay, takeExactMay)
import Text.Megaparsec (Parsec)
import Text.Read (readMaybe)

-- * DatabaseDiscord

-- | The monad transformer stack used to represent computations that work with
-- the database and Discord. The top layer is for Persistent/Esqueleto database
-- operations - the main event handler is run through @runSqlPool@ to leave us
-- with a 'DiscordHandler' for our Discord operations.
--
-- "Tablebot.Plugin.Discord" provides some helper functions for
-- running Discord operations without excessive use of @lift@.
type EnvDatabaseDiscord d = ReaderT d (ReaderT (MVar TablebotCache) (SqlPersistT DiscordHandler))

type DatabaseDiscord = EnvDatabaseDiscord ()

-- | @Database@ The monad transformer stack used to represent computations that work with
-- the just the database for startup actions.
type Database d = SqlPersistM d

data TablebotCache = TCache
  { cacheKnownEmoji :: Map Text Emoji,
    cacheVersionInfo :: VersionInfo
  }

data VersionInfo = VInfo
  { gitHash :: Text,
    procVersion :: Version
  }
  deriving (Show, Eq)

-- * Parser

-- | A simple definition for parsers on Text.
type Parser = Parsec Void Text

liftCache :: ReaderT (MVar TablebotCache) (SqlPersistT DiscordHandler) a -> ReaderT d (ReaderT (MVar TablebotCache) (SqlPersistT DiscordHandler)) a
liftCache = lift

liftSql :: SqlPersistT DiscordHandler a -> ReaderT d (ReaderT (MVar TablebotCache) (SqlPersistT DiscordHandler)) a
liftSql = lift . lift

liftDiscord :: DiscordHandler a -> ReaderT d (ReaderT (MVar TablebotCache) (SqlPersistT DiscordHandler)) a
liftDiscord = lift . lift . lift

-- * Features

-- Bot functionality is split into /features/, which are combined into plugins.
-- Each feature is its own type, and the features are combined via records into
-- full plugins.

-- | For when you get a 'MessageCreate'. Checks that the @name@ is directly
-- after the bot prefix, and then runs @commandParser@ on it.
newtype StartUp d = StartUp
  { -- | An action to run at startup
    startAction :: Database d
  }

-- | For when you get a 'MessageCreate'. Checks that the @name@ is directly
-- after the bot prefix, and then runs @commandParser@ on it.
-- It will first try to match against any subcommands, and if that fails it runs the commandParser
data EnvCommand d = Command
  { -- | The name of the command.
    name :: Text,
    -- | A parser to run on the command arguments, returning a computation to
    -- run in 'DatabaseDiscord'.
    commandParser :: Parser (Message -> EnvDatabaseDiscord d ()),
    -- | A list of subcommands to attempt to parse before the bare command, matching their name.
    subcommands :: [EnvCommand d]
  }

type Command = EnvCommand ()

-- | Construct an aliased command that behaves the same as another command (for things like short forms)
commandAlias :: Text -> EnvCommand d -> EnvCommand d
commandAlias name' (Command _ cp sc) = Command name' cp sc

-- | For when you get a 'MessageCreate', but instead of wanting to match on
-- "!name args" (for prefix "!"), you want a more general match. Useful for
-- commands that work with brackets or look for keywords.
newtype EnvInlineCommand d = InlineCommand
  { -- | The parser to run on every message (non-bot) received.
    inlineCommandParser :: Parser (Message -> EnvDatabaseDiscord d ())
  }

type InlineCommand = EnvInlineCommand ()

-- | How to handle any messages changing. Called on Discord's 'MessageUpdate',
-- 'MessageDelete' and 'MessageDeleteBulk'. Useful for admin bots such as the
-- ub3rbot ub3rlog functionality.
newtype EnvMessageChange d = MessageChange
  { -- | A function to call on every message update. The first argument is
    -- whether the message was updated (True) or deleted (False).
    -- Will be run once per message if bulk deletion occurs.
    onMessageChange :: Bool -> ChannelId -> MessageId -> EnvDatabaseDiscord d ()
  }

type MessageChange = EnvMessageChange ()

-- | Handles added reactions, which is useful for reaction-based functionality
-- (e.g. a quote bot that quotes messages reacted to with a certain emoji).
-- Tied to 'MessageReactionAdd' from Discord.
newtype EnvReactionAdd d = ReactionAdd
  { -- | A function to call on every reaction add, which takes in details of
    -- that reaction ('ReactionInfo').
    onReactionAdd :: ReactionInfo -> EnvDatabaseDiscord d ()
  }

type ReactionAdd = EnvReactionAdd ()

-- | Handles removed reactions, which is useful in the same way as adding
-- reactions. Called on 'MessageReactionRemove'.
newtype EnvReactionDel d = ReactionDel
  { -- | A function to call on every individual reaction delete, which takes in
    -- details of that reaction ('ReactionInfo').
    onReactionDelete :: ReactionInfo -> EnvDatabaseDiscord d ()
  }

type ReactionDel = EnvReactionAdd ()

-- | Handles events not covered by the other kinds of features. This is only
-- relevant to specific admin functionality, such as the deletion of channels.
newtype EnvOther d = Other
  { -- | A function to call on every other event, which takes in details of
    -- that event.
    onOtherEvent :: Event -> EnvDatabaseDiscord d ()
  }

type Other = EnvOther ()

-- | A feature for cron jobs - events which are run every @timeframe@
-- microseconds, regardless of any other interaction with the bot. Useful for
-- things like reminders.
--
-- Note that the loop starts with calling @onCron@ and /then/ delaying, so they
-- will all be invoked on bot start.
data EnvCronJob d = CronJob
  { -- | Delay between each call of @onCron@, in microseconds.
    timeframe :: Int,
    -- | Computation to do with each invocation of this cron job.
    onCron :: EnvDatabaseDiscord d ()
  }

type CronJob = EnvCronJob ()

-- | A feature for generating help text
-- Each help text page consists of a explanation body, as well as a list of sub-pages
-- that display the short text for its page
data HelpPage = HelpPage
  { -- | The [sub]command name
    helpName :: Text,
    -- | List of aliases for this command
    helpAliases :: [Text],
    -- | The text to show when listed in a subpage list. Will be prefixed by its helpName
    helpShortText :: Text,
    -- | The text to show when specifically listed. Appears above the list of subpages
    helpBody :: Text,
    -- | A list of help pages that can be recursively accessed
    helpSubpages :: [HelpPage],
    -- | Permission required to run
    helpPermission :: RequiredPermission
  }
  deriving (Show)

-- | Colour names
-- Colour is a bit of a mess on discord embeds.
-- I've here stolen the pallet list from https://gist.github.com/thomasbnt/b6f455e2c7d743b796917fa3c205f812
data DiscordColour
  = RGB Integer Integer Integer
  | Default
  | Aqua
  | DarkAqua
  | Green
  | DarkGreen
  | Blue
  | DarkBlue
  | Purple
  | DarkPurple
  | LuminousVividPink
  | DarkVividPink
  | Gold
  | DarkGold
  | Orange
  | DarkOrange
  | Red
  | DarkRed
  | Gray
  | DarkGray
  | DarkerGray
  | LightGray
  | Navy
  | DarkNavy
  | Yellow
  | DiscordWhite
  | DiscordBlurple
  | DiscordGrayple
  | DiscordDarkButNotBlack
  | DiscordNotQuiteBlack
  | DiscordGreen
  | DiscordYellow
  | DiscordFuschia
  | DiscordRed
  | DiscordBlack

-- | @hexToRGB@ attempts to convert a potential hex string into its decimal RGB
-- components.
hexToRGB :: String -> Maybe (Integer, Integer, Integer)
hexToRGB hex = do
  let h = map toLower hex
  r <- takeExactMay 2 h >>= toDec
  g <- dropExactMay 2 h >>= takeExactMay 2 >>= toDec
  b <- dropExactMay 4 h >>= toDec
  return (r, g, b)
  where
    toDec :: String -> Maybe Integer
    toDec [s, u] = do
      a <- charToDec s
      b <- charToDec u
      return $ a * 16 + b
    toDec _ = Nothing
    charToDec :: Char -> Maybe Integer
    charToDec 'a' = Just 10
    charToDec 'b' = Just 11
    charToDec 'c' = Just 12
    charToDec 'd' = Just 13
    charToDec 'e' = Just 14
    charToDec 'f' = Just 15
    charToDec c = readMaybe [c]

-- | @hexToDiscordColour@ converts a potential hex string into a DiscordColour,
-- evaluating to Default if it fails.
hexToDiscordColour :: String -> DiscordColour
hexToDiscordColour hex =
  let (r, g, b) = fromMaybe (0, 0, 0) $ hexToRGB hex
   in RGB r g b

-- | Automatic handling of command permissions
-- @UserPermission@ models the current permissions of the user
-- @RequiredPermission@ models the permissions required to run a command.
-- Note, superusers can run all commands
-- -- None: Any user can run the command
-- -- Any: The user must be either an exec, moderator
-- -- Exec: The user must be an exec
-- -- Moderator: The user must be a moderator
-- -- Both: The user must be both an exec and a moderator
-- -- Superuser: The user must be a superuser
data UserPermission = UserPerm
  { permExec :: Bool,
    permModerator :: Bool,
    permSuperuser :: Bool
  }
  deriving (Show, Eq)

data RequiredPermission = None | Any | Exec | Moderator | Both | Superuser deriving (Show, Eq)

-- * Plugins

-- Plugins are groups of features that forms some functionality of your bot.
-- For example, you could have a Reminder bot (see
-- "Tablebot.Plugins.Reminder") that has a command for issuing
-- reminders, a cron job for doing them and then a migration that works with
-- the database.

-- | A plugin. Directly constructing these should be avoided (hence why it is
-- not exported by "Tablebot.Plugin") as this structure could change.
data EnvPlugin d = Pl
  { pluginName :: Text,
    startUp :: StartUp d,
    commands :: [EnvCommand d],
    inlineCommands :: [EnvInlineCommand d],
    onMessageChanges :: [EnvMessageChange d],
    onReactionAdds :: [EnvReactionAdd d],
    onReactionDeletes :: [EnvReactionDel d],
    otherEvents :: [EnvOther d],
    cronJobs :: [EnvCronJob d],
    helpPages :: [HelpPage],
    -- | A list of database migrations generated by Persistance.
    migrations :: [Migration]
  }

type Plugin = EnvPlugin ()

-- | The empty plugin. This is the recommended method for constructing plugins
-- - use record update syntax with this rather than using @Pl@ directly.
--
-- Examples of this in use can be found in the imports of
-- "Tablebot.Plugins".
plug :: Text -> Plugin
plug name' = Pl name' (StartUp (return ())) [] [] [] [] [] [] [] [] []

envPlug :: Text -> StartUp d -> EnvPlugin d
envPlug name' startup = Pl name' startup [] [] [] [] [] [] [] [] []
