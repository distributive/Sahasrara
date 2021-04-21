module Tablebot.Handler.Event (
    parseMessageChange, parseReactionAdd, parseReactionDel, parseOther
) where

import Tablebot.Plugin.Types

import Discord.Types
import Control.Monad

parseMessageChange :: [MessageChange] -> Bool -> ChannelId -> MessageId -> DatabaseDiscord ()
parseMessageChange cs updated cid mid = mapM_ doMessageChange cs
    where doMessageChange c = onMessageChange c updated cid mid

parseReactionAdd :: [ReactionAdd] -> ReactionInfo -> DatabaseDiscord ()
parseReactionAdd cs info = mapM_ doReactionAdd cs
    where doReactionAdd c = onReactionAdd c info

parseReactionDel :: [ReactionDel] -> ReactionInfo -> DatabaseDiscord ()
parseReactionDel cs info = mapM_ doReactionAdd cs
    where doReactionAdd c = onReactionDelete c info

parseOther :: [Other] -> Event -> DatabaseDiscord ()
parseOther cs ev = mapM_ doOther cs
    where doOther c = onOtherEvent c ev