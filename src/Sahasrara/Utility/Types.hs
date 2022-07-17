-- |
-- Module      : Sahasrara.Utility.Types
-- Description : Types used throughout plugins.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- All of the important types used throughout the implementation. Defines plugins,
-- which are made out of features. Also defines how to construct and combine
-- plugins, and the @DatabaseDiscord@ monad transformer stack for allowing
-- database and Discord operations within your features.
module Sahasrara.Utility.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Version.Extra (Version)
import Data.Void (Void)
import Database.Persist.Sqlite (Migration, SqlPersistM, SqlPersistT)
import Discord (DiscordHandler)
import Discord.Interactions
  ( CreateApplicationCommand,
    Interaction,
    InteractionResponseMessage (InteractionResponseMessage),
    InteractionResponseMessageFlag (..),
    InteractionResponseMessageFlags (..),
  )
import Discord.Internal.Rest.Channel (MessageDetailedOpts (MessageDetailedOpts))
import Discord.Types
  ( ActionRow,
    AllowedMentions,
    ApplicationCommandId,
    Attachment,
    ChannelId,
    CreateEmbed,
    Emoji,
    Event (..),
    Message,
    MessageId,
    MessageReference,
    ReactionInfo,
    StickerId,
  )
import Text.Megaparsec (Parsec)

-- * DatabaseDiscord

-- | The monad transformer stack used to represent computations that work with
-- the database and Discord. The top layer is for Persistent/Esqueleto database
-- operations - the main event handler is run through @runSqlPool@ to leave us
-- with a 'DiscordHandler' for our Discord operations.
--
-- "Sahasrara.Plugin.Discord" provides some helper functions for
-- running Discord operations without excessive use of @lift@.
type EnvDatabaseDiscord d = ReaderT d (ReaderT (MVar SahasraraCache) (SqlPersistT DiscordHandler))

type DatabaseDiscord = EnvDatabaseDiscord ()

-- | @Database@ The monad transformer stack used to represent computations that work with
-- the just the database for startup actions.
type Database d = SqlPersistM d

data SahasraraCache = TCache
  { cacheKnownEmoji :: Map Text Emoji,
    cacheApplicationCommands :: Map ApplicationCommandId (Interaction -> EnvDatabaseDiscord () ()),
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

liftCache :: ReaderT (MVar SahasraraCache) (SqlPersistT DiscordHandler) a -> ReaderT d (ReaderT (MVar SahasraraCache) (SqlPersistT DiscordHandler)) a
liftCache = lift

liftSql :: SqlPersistT DiscordHandler a -> ReaderT d (ReaderT (MVar SahasraraCache) (SqlPersistT DiscordHandler)) a
liftSql = lift . lift

liftDiscord :: DiscordHandler a -> ReaderT d (ReaderT (MVar SahasraraCache) (SqlPersistT DiscordHandler)) a
liftDiscord = lift . lift . lift

-- * Features

-- Bot functionality is split into /features/, which are combined into plugins.
-- Each feature is its own type, and the features are combined via records into
-- full plugins.

-- | For when the plugin is first used, to ensure that certain data is
-- available.
newtype StartUp d = StartUp
  { -- | An action to run at startup
    startAction :: Database d
  }

-- | For when you get a 'MessageCreate'. Checks that the @name@ is directly
-- after the bot prefix, and then runs @commandParser@ on it.
-- It will first try to match against any subcommands, and if that fails it runs
-- the commandParser.
data EnvCommand d = Command
  { -- | The name of the command.
    commandName :: Text,
    -- | A parser to run on the command arguments, returning a computation to
    -- run in 'DatabaseDiscord'.
    commandParser :: Parser (Message -> EnvDatabaseDiscord d ()),
    -- | A list of subcommands to attempt to parse before the bare command,
    -- matching their name.
    commandSubcommands :: [EnvCommand d]
  }

type Command = EnvCommand ()

-- | Construct an aliased command that behaves the same as another command (for
-- things like short forms).
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

type ReactionDel = EnvReactionDel ()

-- | Handles the creation of an application command and of the action to be
-- performed once that application command is received.
--
-- This handles things like chat input (slash commands), message commands, or
-- user commands. The `applicationCommand` is the data structure that
-- represents the application command, and the `applicationCommandRecv` is the
-- action to be performed when this application command is received.
data EnvApplicationCommandRecv d = ApplicationCommandRecv
  { -- | The application command to be created.
    applicationCommand :: CreateApplicationCommand,
    -- | The action to run when the application command is received.
    applicationCommandRecv :: Interaction -> EnvDatabaseDiscord d ()
  }

type ApplicationCommandRecv = EnvApplicationCommandRecv ()

-- | Handles recieving of components, such as buttons or select menus.
--
-- The name is the name of the component within a plugin. Choose something
-- unique within the plugin.
data EnvComponentRecv d = ComponentRecv
  { componentName :: Text,
    onComponentRecv :: Interaction -> EnvDatabaseDiscord d ()
  }

type ComponentRecv = EnvComponentRecv ()

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
-- Each help text page consists of a explanation body, as well as a list of
-- sub-pages that display the short text for its page
data HelpPage = HelpPage
  { -- | The [sub]command name
    helpName :: Text,
    -- | List of aliases for this command
    helpAliases :: [Text],
    -- | The text to show when listed in a subpage list. Will be prefixed by its
    -- helpName
    helpShortText :: Text,
    -- | The text to show when specifically listed. Appears above the list of
    -- subpages
    helpBody :: Text,
    -- | A list of help pages that can be recursively accessed
    helpSubpages :: [HelpPage],
    -- | Permission required to run
    helpPermission :: RequiredPermission
  }
  deriving (Show)

-- | Automatic handling of command permissions
-- @UserPermission@ models the current permissions of the user
-- @RequiredPermission@ models the permissions required to run a command.
-- Note, superusers can run all commands
-- -- None: Any user can run the command
-- -- Moderator: The user must be a moderator
-- -- Superuser: The user must be a superuser
data UserPermission = UserPerm
  { permModerator :: Bool,
    permSuperuser :: Bool
  }
  deriving (Show, Eq)

data RequiredPermission = None | Any | Exec | Moderator | Both | Superuser deriving (Show, Eq)

-- * Plugins

-- Plugins are groups of features that forms some functionality of your bot.
-- For example, you could have a Reminder bot (see
-- "Sahasrara.Plugins.Reminder") that has a command for issuing
-- reminders, a cron job for doing them and then a migration that works with
-- the database.

-- | A plugin. Directly constructing these should be avoided (hence why it is
-- not exported by "Sahasrara.Plugin") as this structure could change.
data EnvPlugin d = Pl
  { pluginName :: Text,
    startUp :: StartUp d,
    applicationCommands :: [EnvApplicationCommandRecv d],
    commands :: [EnvCommand d],
    inlineCommands :: [EnvInlineCommand d],
    onMessageChanges :: [EnvMessageChange d],
    onReactionAdds :: [EnvReactionAdd d],
    onReactionDeletes :: [EnvReactionDel d],
    onComponentRecvs :: [EnvComponentRecv d],
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
-- "Sahasrara.Plugins".
plug :: Text -> Plugin
plug name' = Pl name' (StartUp (return ())) [] [] [] [] [] [] [] [] [] [] []

envPlug :: Text -> StartUp d -> EnvPlugin d
envPlug name' startup = Pl name' startup [] [] [] [] [] [] [] [] [] [] []

messageDetailsBasic :: Text -> MessageDetails
messageDetailsBasic t = MessageDetails Nothing (Just t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Default MessageDetails where
  def = MessageDetails Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | This data structure as a convenient way to make either interaction responses
-- or just plain messages. It is used in cases that we're either gonna return
-- an interaction or a message.
data MessageDetails = MessageDetails
  { messageDetailsTTS :: Maybe Bool,
    messageDetailsContent :: Maybe Text,
    messageDetailsEmbeds :: Maybe [CreateEmbed],
    messageDetailsFile :: Maybe (Text, ByteString),
    messageDetailsAllowedMentions :: Maybe AllowedMentions,
    messageDetailsFlags :: Maybe InteractionResponseMessageFlags,
    messageDetailsReference :: Maybe MessageReference,
    messageDetailsComponents :: Maybe [ActionRow],
    messageDetailsAttachments :: Maybe [Attachment],
    messageDetailsStickerIds :: Maybe [StickerId]
  }
  deriving (Show)

makeEphermeral :: MessageDetails -> MessageDetails
makeEphermeral m = m {messageDetailsFlags = Just $ InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral]}

convertMessageFormatInteraction :: MessageDetails -> InteractionResponseMessage
convertMessageFormatInteraction MessageDetails {..} =
  InteractionResponseMessage
    messageDetailsTTS
    messageDetailsContent
    messageDetailsEmbeds
    messageDetailsAllowedMentions
    messageDetailsFlags
    messageDetailsComponents
    messageDetailsAttachments

convertMessageFormatBasic :: MessageDetails -> MessageDetailedOpts
convertMessageFormatBasic MessageDetails {..} =
  MessageDetailedOpts
    (fromMaybe "" messageDetailsContent)
    (fromMaybe False messageDetailsTTS)
    messageDetailsEmbeds
    messageDetailsFile
    messageDetailsAllowedMentions
    messageDetailsReference
    messageDetailsComponents
    messageDetailsStickerIds
