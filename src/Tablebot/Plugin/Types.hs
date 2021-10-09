-- |
-- Module      : Tablebot.Plugin.Types
-- Description : Types used throughout plugins.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- All of the important types used throughout the implementation. Defines plugins,
-- which are made out of features. Also defines how to construct and combine
-- plugins, and the @DatabaseDiscord@ monad transformer stack for allowing
-- database and Discord operations within your features.
module Tablebot.Plugin.Types where

import Data.Text (Text)
import Data.Void (Void)
import Database.Persist.Sqlite (Migration, SqlPersistT)
import Discord (DiscordHandler)
import Discord.Types
  ( ChannelId,
    Event (..),
    Message,
    MessageId,
    ReactionInfo,
  )
import Text.Megaparsec (Parsec)

-- * DatabaseDiscord

-- | The monad transformer stack used to represent computations that work with
-- the database and Discord. The top layer is for Persistent/Esqueleto database
-- operations - the main event handler is run through @runSqlPool@ to leave us
-- with a 'DiscordHandler' for our Discord operations.
--
-- "Tablebot.Plugin.Discord" provides some helper functions for
-- running Discord operations without excessive use of @lift@.
type DatabaseDiscord = SqlPersistT DiscordHandler

-- * Parser

-- | A simple definition for parsers on Text.
type Parser = Parsec Void Text

-- * Features

-- Bot functionality is split into /features/, which are combined into plugins.
-- Each feature is its own type, and the features are combined via records into
-- full plugins.

-- | For when you get a 'MessageCreate'. Checks that the @name@ is directly
-- after the bot prefix, and then runs @commandParser@ on it.
data Command = Command
  { -- | The name of the command.
    name :: Text,
    -- | A parser to run on the command arguments, returning a computation to
    -- run in 'DatabaseDiscord'.
    commandParser :: Parser (Message -> DatabaseDiscord ())
  }

-- | For when you get a 'MessageCreate', but instead of wanting to match on
-- "!name args" (for prefix "!"), you want a more general match. Useful for
-- commands that work with brackets or look for keywords.
newtype InlineCommand = InlineCommand
  { -- | The parser to run on every message (non-bot) received.
    inlineCommandParser :: Parser (Message -> DatabaseDiscord ())
  }

-- | How to handle any messages changing. Called on Discord's 'MessageUpdate',
-- 'MessageDelete' and 'MessageDeleteBulk'. Useful for admin bots such as the
-- ub3rbot ub3rlog functionality.
newtype MessageChange = MessageChange
  { -- | A function to call on every message update. The first argument is
    -- whether the message was updated (True) or deleted (False).
    -- Will be run once per message if bulk deletion occurs.
    onMessageChange :: Bool -> ChannelId -> MessageId -> DatabaseDiscord ()
  }

-- | Handles added reactions, which is useful for reaction-based functionality
-- (e.g. a quote bot that quotes messages reacted to with a certain emoji).
-- Tied to 'MessageReactionAdd' from Discord.
newtype ReactionAdd = ReactionAdd
  { -- | A function to call on every reaction add, which takes in details of
    -- that reaction ('ReactionInfo').
    onReactionAdd :: ReactionInfo -> DatabaseDiscord ()
  }

-- | Handles removed reactions, which is useful in the same way as adding
-- reactions. Called on 'MessageReactionRemove'.
newtype ReactionDel = ReactionDel
  { -- | A function to call on every individual reaction delete, which takes in
    -- details of that reaction ('ReactionInfo').
    onReactionDelete :: ReactionInfo -> DatabaseDiscord ()
  }

-- | Handles events not covered by the other kinds of features. This is only
-- relevant to specific admin functionality, such as the deletion of channels.
newtype Other = Other
  { -- | A function to call on every other event, which takes in details of
    -- that event.
    onOtherEvent :: Event -> DatabaseDiscord ()
  }

-- | A feature for cron jobs - events which are run every @timeframe@
-- microseconds, regardless of any other interaction with the bot. Useful for
-- things like reminders.
--
-- Note that the loop starts with calling @onCron@ and /then/ delaying, so they
-- will all be invoked on bot start.
data CronJob = CronJob
  { -- | Delay between each call of @onCron@, in microseconds.
    timeframe :: Int,
    -- | Computation to do with each invocation of this cron job.
    onCron :: DatabaseDiscord ()
  }

-- | A feature for generating help text
-- Each help text page consists of a explanation body, as well as a list of sub-pages
-- that display the short text for its page
data HelpPage = HelpPage {
    -- | The [sub]command name
    helpName :: Text,
    -- | The text to show when listed in a subpage list. Will be prefixed by its helpName
    helpShortText :: Text,
    -- | The text to show when specifically listed. Appears above the list of subpages
    helpBody :: Text,
    -- | A list of help pages that can be recursively accessed
    helpSubpages :: [HelpPage]
} deriving Show

-- * Plugins

-- Plugins are groups of features that forms some functionality of your bot.
-- For example, you could have a Reminder bot (see
-- "Tablebot.Plugins.Reminder") that has a command for issuing
-- reminders, a cron job for doing them and then a migration that works with
-- the database.

-- | A plugin. Directly constructing these should be avoided (hence why it is
-- not exported by "Tablebot.Plugin") as this structure could change.
data Plugin = Pl
  { commands :: [Command],
    inlineCommands :: [InlineCommand],
    onMessageChanges :: [MessageChange],
    onReactionAdds :: [ReactionAdd],
    onReactionDeletes :: [ReactionDel],
    otherEvents :: [Other],
    cronJobs :: [CronJob],
    helpPages :: [HelpPage],
    -- | A list of database migrations generated by Persistance.
    migrations :: [Migration]
  }

-- | The empty plugin. This is the recommended method for constructing plugins
-- - use record update syntax with this rather than using @Pl@ directly.
--
-- Examples of this in use can be found in the imports of
-- "Tablebot.Plugins".
plug :: Plugin
plug = Pl [] [] [] [] [] [] [] [] []

-- | Combines a list of plugins into a single plugin with the combined
-- functionality. The bot actually runs a single plugin, which is just the
-- combined version of all input plugins.
combinePlugins :: [Plugin] -> Plugin
combinePlugins [] = plug
combinePlugins (p : ps) =
  let p' = combinePlugins ps
   in Pl
        { commands = merge commands p p',
          inlineCommands = merge inlineCommands p p',
          onMessageChanges = merge onMessageChanges p p',
          onReactionAdds = merge onReactionAdds p p',
          onReactionDeletes = merge onReactionDeletes p p',
          otherEvents = merge otherEvents p p',
          cronJobs = merge cronJobs p p',
          migrations = merge migrations p p',
          helpPages = merge helpPages p p'
        }
  where
    merge f p p' = f p +++ f p'
    -- We expect empty list to be very common in this process, so we add
    -- the special case where the second element is empty. This is
    -- because plugins are unlikely to define every possible kind of
    -- event, so we will get many [] instances.
    (+++) :: [a] -> [a] -> [a]
    [] +++ ys = ys
    xs +++ [] = xs
    (x : xs) +++ ys = x : xs +++ ys
