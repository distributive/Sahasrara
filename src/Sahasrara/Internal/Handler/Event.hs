-- |
-- Module      : Sahasrara.Internal.Handler.Event
-- Description : The event handler for everything else.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This module deals with other kinds of features - 'MessageChange',
-- 'ReactionAdd', 'ReactionDel' and 'Other'.
module Sahasrara.Internal.Handler.Event
  ( parseMessageChange,
    parseReactionAdd,
    parseReactionDel,
    parseComponentRecv,
    parseApplicationCommandRecv,
    parseOther,
  )
where

import Control.Concurrent (readMVar)
import Control.Monad.RWS (MonadIO (liftIO), MonadReader (ask))
import qualified Data.Map as M
import Data.Text as T (drop, isPrefixOf, length)
import Discord.Interactions (ApplicationCommandData (applicationCommandDataId), ComponentData (componentDataCustomId), Interaction (..))
import Discord.Types (ChannelId, Event, MessageId, ReactionInfo)
import Sahasrara.Internal.Plugins (changeAction)
import Sahasrara.Internal.Types as IT
import Sahasrara.Utility.Exception (BotException (InteractionException), throwBot)
import qualified Sahasrara.Utility.Types as UT

-- | This runs each 'MessageChange' feature in @cs@ with the information from a
-- Discord 'MessageUpdate' or 'MessageDelete' event - whether it is an update
-- or delete (the bool @updated@), the 'ChannelId' @cid@, and the 'MessageId'
-- @mid@.
parseMessageChange ::
  [CompiledMessageChange] ->
  Bool ->
  ChannelId ->
  MessageId ->
  CompiledDatabaseDiscord ()
parseMessageChange cs updated cid mid = mapM_ doMessageChange cs
  where
    doMessageChange c = onMessageChange c updated cid mid

-- | This runs each 'ReactionAdd' feature in @cs@ with the information from a
-- Discord 'MessageReactionAdd' event provided as 'ReactionInfo' @info@.
parseReactionAdd :: [CompiledReactionAdd] -> ReactionInfo -> CompiledDatabaseDiscord ()
parseReactionAdd cs info = mapM_ doReactionAdd cs
  where
    doReactionAdd c = onReactionAdd c info

-- | This runs each 'ReactionDel' feature in @cs@ with the information from
-- a Discord 'MessageReactionRemove' event provided as 'ReactionInfo' @info@.
parseReactionDel :: [CompiledReactionDel] -> ReactionInfo -> CompiledDatabaseDiscord ()
parseReactionDel cs info = mapM_ doReactionAdd cs
  where
    doReactionAdd c = onReactionDelete c info

-- | When given the compiled component recv actions and a component interaction,
-- find and run the correct action.
parseComponentRecv :: [CompiledComponentRecv] -> Interaction -> CompiledDatabaseDiscord ()
parseComponentRecv cs info@InteractionComponent {componentData = idc} = mapM_ removePrefix cs'
  where
    getPrefix ccr = componentPluginName ccr <> " " <> componentName ccr
    cs' = filter (\ccr -> getPrefix ccr `isPrefixOf` componentDataCustomId idc) cs
    removePrefix ccr = ccr `onComponentRecv` (info {componentData = (idc {componentDataCustomId = T.drop (T.length (getPrefix ccr)) (componentDataCustomId idc)})})
parseComponentRecv _ _ = return ()

-- | When given an application command interaction, find and run the correct
-- action.
parseApplicationCommandRecv :: Interaction -> CompiledDatabaseDiscord ()
parseApplicationCommandRecv info@InteractionApplicationCommand {applicationCommandData = idac} = do
  tvar <- ask
  cache <- liftIO $ readMVar tvar
  let action = UT.cacheApplicationCommands cache M.!? applicationCommandDataId idac
  case action of
    Nothing -> throwBot $ InteractionException "could not find the given application command"
    Just act -> changeAction () $ act info
parseApplicationCommandRecv info@InteractionApplicationCommandAutocomplete {applicationCommandData = idac} = do
  tvar <- ask
  cache <- liftIO $ readMVar tvar
  let action = UT.cacheApplicationCommands cache M.!? applicationCommandDataId idac
  case action of
    Nothing -> throwBot $ InteractionException "could not find the given application command"
    Just act -> changeAction () $ act info
parseApplicationCommandRecv _ = return ()

-- | This runs each 'Other' feature in @cs@ with the Discord 'Event' provided.
-- Note that any events covered by other feature types will /not/ be run
-- through this.
parseOther :: [CompiledOther] -> Event -> CompiledDatabaseDiscord ()
parseOther cs ev = mapM_ doOther cs
  where
    doOther c = onOtherEvent c ev
