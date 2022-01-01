-- |
-- Module      : Tablebot.Internal.Handler.Event
-- Description : The event handler for everything else.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module deals with other kinds of features - 'MessageChange',
-- 'ReactionAdd', 'ReactionDel' and 'Other'.
module Tablebot.Internal.Handler.Event
  ( parseMessageChange,
    parseReactionAdd,
    parseReactionDel,
    parseOther,
  )
where

import Discord.Types (ChannelId, Event, MessageId, ReactionInfo)
import Tablebot.Internal.Types

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

-- | This runs each 'Other' feature in @cs@ with the Discord 'Event' provided.
-- Note that any events covered by other feature types will /not/ be run
-- through this.
parseOther :: [CompiledOther] -> Event -> CompiledDatabaseDiscord ()
parseOther cs ev = mapM_ doOther cs
  where
    doOther c = onOtherEvent c ev
