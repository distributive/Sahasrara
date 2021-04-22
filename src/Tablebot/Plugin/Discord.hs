{-|
Module      : Tablebot.Plugin.Discord
Description : Discord helpers for building plugins.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

This module contains helpful Discord functionality for building plugins
without having to lift Discord operations constantly.
-}
module Tablebot.Plugin.Discord (
    sendMessage, sendMessageVoid, reactToMessage, getMessage, Message
) where

import Tablebot.Plugin (DatabaseDiscord)

import Discord (restCall, RestCallErrorCode)
import Discord.Types
import qualified Discord.Requests as R
import Data.Text
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(lift))

-- | @sendMessage@ sends the input message @t@ in the same channel as message
-- @m@. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
sendMessage :: Message -> Text
    -> DatabaseDiscord (Either RestCallErrorCode Message)
sendMessage m t = lift . restCall $ R.CreateMessage (messageChannel m) t

-- | @sendMessageVoid@ does the same as @sendMessage@, except it does not
-- return anything. Useful if you don't care whether a message successfully
-- sent or not.
sendMessageVoid :: Message -> Text -> DatabaseDiscord ()
sendMessageVoid m t = void $ sendMessage m t

-- | @getMessage@ gets the relevant 'Message' object for a given 'ChannelId'
-- and 'MessageId', or returns an error ('RestCallErrorCode').
getMessage :: ChannelId -> MessageId
    -> DatabaseDiscord (Either RestCallErrorCode Message)
getMessage cid mid = lift . restCall $ R.GetChannelMessage (cid, mid)

-- | @reactToMessage@ reacts to the given message with the emoji specified
-- by the text input (see README.md from discord-haskell). Returns @()@ if
-- successful, else the error ('RestCallErrorCode').
reactToMessage :: Message -> Text
    -> DatabaseDiscord (Either RestCallErrorCode ())
reactToMessage m e = lift . restCall $
    R.CreateReaction (messageChannel m, messageId m) e