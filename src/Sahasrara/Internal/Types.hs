-- |
-- Module      : Sahasrara.Internal.Types
-- Description : Non-public types used throughout the rest of the implementation.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- All of the important types used throughout the implementation that aren't exposed to plugins.
-- Defines a @CompiledPlugin@, which represents a compiled and unified form of a plugin to
-- allow homogeneous storage throughout the rest of the implementation.
module Sahasrara.Internal.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Reader (ReaderT)
import Data.Default (Default)
import Data.Text (Text)
import Database.Persist.Sqlite (Migration, SqlPersistT)
import Discord
import Discord.Interactions (CreateApplicationCommand, Interaction)
import Discord.Types
import Sahasrara.Utility.Types

type CompiledDatabaseDiscord = ReaderT (MVar SahasraraCache) (SqlPersistT DiscordHandler)

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
  { compiledApplicationCommands :: [CompiledApplicationCommand],
    compiledCommands :: [CompiledCommand],
    compiledInlineCommands :: [CompiledInlineCommand],
    compiledOnMessageChanges :: [CompiledMessageChange],
    compiledOnReactionAdds :: [CompiledReactionAdd],
    compiledOnReactionDeletes :: [CompiledReactionDel],
    compiledOnComponentRecvs :: [CompiledComponentRecv],
    compiledOtherEvents :: [CompiledOther],
    compiledCronJobs :: [CompiledCronJob]
  }

instance Default PluginActions where
  def = PA [] [] [] [] [] [] [] [] []

data CombinedPlugin = CmPl
  { combinedSetupAction :: [Database PluginActions],
    combinedHelpPages :: [HelpPage],
    combinedMigrations :: [Migration]
  }

instance Default CombinedPlugin where
  def = CmPl [] [] []

-- * Compiled Items

-- These are compiled forms of the actions from the public types that remove the reader.

data CompiledApplicationCommand = CApplicationCommand
  { applicationCommand :: CreateApplicationCommand,
    applicationCommandAction :: Interaction -> CompiledDatabaseDiscord ()
  }

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

data CompiledComponentRecv = CComponentRecv
  { componentPluginName :: Text,
    componentName :: Text,
    onComponentRecv :: Interaction -> CompiledDatabaseDiscord ()
  }

newtype CompiledOther = COther
  { onOtherEvent :: Event -> CompiledDatabaseDiscord ()
  }

data CompiledCronJob = CCronJob
  { timeframe :: Int,
    onCron :: CompiledDatabaseDiscord ()
  }

-- * Configuration type

-- Allows others to configure the bot.

data BotConfig = BotConfig
  { rootHelpText :: Text,
    gamePlaying :: Text
  }

instance Default BotConfig where
  def =
    BotConfig
      { rootHelpText = "This bot is built off the Tablebot framework (<https://github.com/WarwickTabletop/tablebot>).",
        gamePlaying = "Kirby: Planet Robobot"
      }
