-- |
-- Module      : Tablebot.Internal.Types
-- Description : Non-public types used throughout the rest of the implementation.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- All of the important types used throughout the implementation that aren't exposed to plugins.
-- Defines a @CompiledPlugin@, which represents a compiled and unified form of a plugin to
-- allow homogeneous storage throughout the rest of the implementation.
module Tablebot.Internal.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Database.Persist.Sqlite (Migration, SqlPersistT)
import Discord
import Discord.Types
import Tablebot.Utility.Types

type CompiledDatabaseDiscord = ReaderT (MVar TablebotCache) (SqlPersistT DiscordHandler)

-- | @CompiledPlugin@ represents the internal format of the plugins.
-- Its main job is to convert all the plugins into one type by collapsing
-- it down to a single action that returns the respective actions
data CompiledPlugin = CPl
  { compiledName :: Text,
    setupAction :: Database PluginActions,
    helpPages :: [HelpPage],
    migrations :: [Migration]
  }

data PluginActions = PA
  { compiledCommands :: [CompiledCommand],
    compiledInlineCommands :: [CompiledInlineCommand],
    compiledOnMessageChanges :: [CompiledMessageChange],
    compiledOnReactionAdds :: [CompiledReactionAdd],
    compiledOnReactionDeletes :: [CompiledReactionDel],
    compiledOtherEvents :: [CompiledOther],
    compiledCronJobs :: [CompiledCronJob]
  }

data CombinedPlugin = CmPl
  { combinedSetupAction :: [Database PluginActions],
    combinedHelpPages :: [HelpPage],
    combinedMigrations :: [Migration]
  }

-- * Compiled Items

-- These are compiled forms of the actions from the public types that remove the reader.

data CompiledCommand = CCommand
  { commandName :: Text,
    commandParser :: Parser (Message -> CompiledDatabaseDiscord ()),
    commandSubcommands :: [CompiledCommand]
  }

newtype CompiledInlineCommand = CInlineCommand
  { inlineCommandParser :: Parser (Message -> CompiledDatabaseDiscord ())
  }

newtype CompiledMessageChange = CMessageChange
  { onMessageChange :: Bool -> ChannelId -> MessageId -> CompiledDatabaseDiscord ()
  }

newtype CompiledReactionAdd = CReactionAdd
  { onReactionAdd :: ReactionInfo -> CompiledDatabaseDiscord ()
  }

newtype CompiledReactionDel = CReactionDel
  { onReactionDelete :: ReactionInfo -> CompiledDatabaseDiscord ()
  }

newtype CompiledOther = COther
  { onOtherEvent :: Event -> CompiledDatabaseDiscord ()
  }

data CompiledCronJob = CCronJob
  { timeframe :: Int,
    onCron :: CompiledDatabaseDiscord ()
  }
