-- |
-- Module      : Sahasrara.Internal.Plugins
-- Description : Some internal code for handling plugins
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This contains some functions to combine and compile plugins
module Sahasrara.Internal.Plugins where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Default (Default (def))
import Discord.Types (Message)
import Sahasrara.Internal.Types hiding (helpPages, migrations)
import qualified Sahasrara.Internal.Types as IT
import Sahasrara.Utility.Types as UT

-- | Combines a list of plugins into a single plugin with the combined
-- functionality. The bot actually runs a single plugin, which is just the
-- combined version of all input plugins.
combinePlugins :: [CompiledPlugin] -> CombinedPlugin
combinePlugins [] = def
combinePlugins (p : ps) =
  let p' = combinePlugins ps
   in CmPl
        { combinedSetupAction = setupAction p : combinedSetupAction p',
          combinedMigrations = IT.migrations p ++ combinedMigrations p',
          combinedHelpPages = IT.helpPages p ++ combinedHelpPages p'
        }

-- | Combines a list of plugins actions into a single pa with the combined
-- functionality.
combineActions :: [PluginActions] -> PluginActions
combineActions [] = def
combineActions (p : ps) =
  let p' = combineActions ps
   in PA
        { compiledApplicationCommands = compiledApplicationCommands p +++ compiledApplicationCommands p',
          compiledCommands = compiledCommands p +++ compiledCommands p',
          compiledInlineCommands = compiledInlineCommands p +++ compiledInlineCommands p',
          compiledOnMessageChanges = compiledOnMessageChanges p +++ compiledOnMessageChanges p',
          compiledOnReactionAdds = compiledOnReactionAdds p +++ compiledOnReactionAdds p',
          compiledOnReactionDeletes = compiledOnReactionDeletes p +++ compiledOnReactionDeletes p',
          compiledOnComponentRecvs = compiledOnComponentRecvs p +++ compiledOnComponentRecvs p',
          compiledOtherEvents = compiledOtherEvents p +++ compiledOtherEvents p',
          compiledCronJobs = compiledCronJobs p +++ compiledCronJobs p'
        }
  where
    -- copy across Finnbar's +++ optimisation for empty lists from the old system, as it applies here.
    [] +++ [] = []
    a +++ [] = a
    [] +++ a = a
    a +++ b = a ++ b

compilePlugin :: EnvPlugin b -> CompiledPlugin
compilePlugin p = CPl (pluginName p) sa (helpPages p) (migrations p)
  where
    sa :: Database PluginActions
    sa = do
      state <- startAction (startUp p)

      return $
        PA
          (map (fixApplicationCommand state) $ applicationCommands p)
          (map (fixCommand state) $ commands p)
          (map (fixInlineCommand state) $ inlineCommands p)
          (map (fixOnMessageChanges state) $ onMessageChanges p)
          (map (fixOnReactionAdd state) $ onReactionAdds p)
          (map (fixOnReactionDelete state) $ onReactionDeletes p)
          (map (fixOnComponentRecv state) $ onComponentRecvs p)
          (map (fixOther state) $ otherEvents p)
          (map (fixCron state) $ cronJobs p)

    -- Command converters
    fixApplicationCommand state' (ApplicationCommandRecv cac action') = CApplicationCommand cac (changeAction state' . action')
    fixCommand state' (Command name' action' subcommands') = CCommand name' (compileParser state' action') (map (fixCommand state') subcommands')
    fixInlineCommand state' (InlineCommand action') = CInlineCommand (compileParser state' action')
    fixOnMessageChanges state' (MessageChange action') = CMessageChange (((changeAction state' .) .) . action')
    fixOnReactionAdd state' (ReactionAdd action') = CReactionAdd (changeAction state' . action')
    fixOnReactionDelete state' (ReactionDel action') = CReactionDel (changeAction state' . action')
    fixOnComponentRecv state' (ComponentRecv name' action') = CComponentRecv (pluginName p) name' (changeAction state' . action')
    fixOther state' (Other action') = COther (changeAction state' . action')
    fixCron state' (CronJob time action') = CCronJob time (changeAction state' action')

-- * Helper converters

compileParser :: s -> Parser (Message -> EnvDatabaseDiscord s a) -> Parser (Message -> CompiledDatabaseDiscord a)
compileParser s = fmap (changeMessageAction s)

changeMessageAction :: s -> (Message -> EnvDatabaseDiscord s a) -> Message -> CompiledDatabaseDiscord a
changeMessageAction = changeAnyAction

changeAnyAction :: s -> (m -> EnvDatabaseDiscord s a) -> m -> CompiledDatabaseDiscord a
changeAnyAction s action m = changeAction s (action m)

changeAction :: s -> EnvDatabaseDiscord s a -> CompiledDatabaseDiscord a
changeAction s action = runReaderT action s
