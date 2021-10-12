-- |
-- Module      : Tablebot.Plugin.Discord
-- Description : Discord helpers for building plugins.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains helpful Discord functionality for building plugins
-- without having to lift Discord operations constantly.
module Tablebot.Plugin.Discord
  ( sendMessage,
    sendMessageVoid,
    sendEmbedMessage,
    sendEmbedMessageVoid,
    reactToMessage,
    getMessage,
    getMessageMember,
    getReplyMessage,
    getPrecedingMessage,
    toMention,
    toMentionStr,
    getMessageLink,
    Message,
  )
where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Text
import Discord (RestCallErrorCode, restCall)
import qualified Discord.Requests as R
import Discord.Types
import Tablebot.Handler.Embed
import Tablebot.Plugin (DatabaseDiscord)

-- | @sendMessage@ sends the input message @t@ in the same channel as message
-- @m@. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
sendMessage ::
  Message ->
  Text ->
  DatabaseDiscord (Either RestCallErrorCode Message)
sendMessage m t = lift . restCall $ R.CreateMessage (messageChannel m) t

-- | @sendEmbedMessage@ sends the input message @t@ in the same channel as message
-- @m@ with an additional full Embed. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
-- This is *really* janky. The library exposes *no way* to create a coloured embed through its main api,
-- so I'm having to manually reimplement the sending logic just to add this in.
-- If you suffer from nightmares, don't look in 'Tablebot.Handler.Embed'. Nothing good lives there.
-- In the future, I may actually submit a PR to discord-haskell with a fix to allow colours properly.
sendEmbedMessage ::
  Embeddable e =>
  Message ->
  Text ->
  e ->
  DatabaseDiscord (Either RestCallErrorCode Message)
sendEmbedMessage m t e = lift . restCall $ TablebotEmbedRequest (messageChannel m) t (asEmbed e)

-- | @sendMessageVoid@ does the same as @sendMessage@, except it does not
-- return anything. Useful if you don't care whether a message successfully
-- sent or not.
sendMessageVoid :: Message -> Text -> DatabaseDiscord ()
sendMessageVoid m t = void $ sendMessage m t

-- | @sendEmbedMessageVoid@ does the same as @sendEmbedMessage@, except it does not
-- return anything. Useful if you don't care whether a message successfully
-- sent or not.
sendEmbedMessageVoid :: Embeddable e => Message -> Text -> e -> DatabaseDiscord ()
sendEmbedMessageVoid m t e = void $ sendEmbedMessage m t e

-- | @getMessage@ gets the relevant 'Message' object for a given 'ChannelId'
-- and 'MessageId', or returns an error ('RestCallErrorCode').
getMessage ::
  ChannelId ->
  MessageId ->
  DatabaseDiscord (Either RestCallErrorCode Message)
getMessage cid mid = lift . restCall $ R.GetChannelMessage (cid, mid)

-- | @reactToMessage@ reacts to the given message with the emoji specified
-- by the text input (see README.md from discord-haskell). Returns @()@ if
-- successful, else the error ('RestCallErrorCode').
reactToMessage ::
  Message ->
  Text ->
  DatabaseDiscord (Either RestCallErrorCode ())
reactToMessage m e =
  lift . restCall $
    R.CreateReaction (messageChannel m, messageId m) e

-- | @getReplyMessage@ returns the message being replied to (if applicable)
getReplyMessage :: Message -> DatabaseDiscord (Maybe Message)
getReplyMessage m = do
  let m' = referencedMessage m
  let mRef = messageReference m
  case m' of
    Just msg -> return $ Just msg
    Nothing -> case mRef of
      Nothing -> return Nothing
      Just mRef' -> maybeGetMessage (referenceChannelId mRef') (referenceMessageId mRef')
  where
    maybeGetMessage :: Maybe ChannelId -> Maybe MessageId -> DatabaseDiscord (Maybe Message)
    maybeGetMessage (Just cId) (Just mId) = do
      m' <- getMessage cId mId
      case m' of
        Left _ -> return Nothing
        Right msg -> return $ Just msg
    maybeGetMessage _ _ = return Nothing

-- | @getPrecedingMessage@ returns the message immediately above the provided message
getPrecedingMessage :: Message -> DatabaseDiscord (Maybe Message)
getPrecedingMessage m = do
  mlst <- lift . restCall $ R.GetChannelMessages (messageChannel m) (1, R.BeforeMessage (messageId m))
  case mlst of
    Right mlst' ->
      return $ Just $ Prelude.head mlst'
    Left _ -> return Nothing

-- | @getMessageMember@ returns the message member object if it was sent from a Discord server,
-- or @Nothing@ if it was sent from a DM (or the API fails)
getMessageMember :: Message -> DatabaseDiscord (Maybe GuildMember)
getMessageMember m = gMM (messageGuild m) m
  where
    maybeRight :: Either a b -> Maybe b
    maybeRight (Left _) = Nothing
    maybeRight (Right a) = Just a
    gMM :: Maybe GuildId -> Message -> DatabaseDiscord (Maybe GuildMember)
    gMM Nothing _ = return Nothing
    gMM (Just g') m' = do
      a <- lift $ restCall $ R.GetGuildMember g' (userId $ messageAuthor m')
      return $ maybeRight a

-- | @toMention@ converts a user to its corresponding mention
toMention :: User -> Text
toMention u = pack $ toMentionStr u

-- | @toMentionStr@ converts a user to its corresponding mention, returning a string to prevent packing and unpacking
toMentionStr :: User -> String
toMentionStr u = "<@!" ++ show (userId u) ++ ">"

getMessageLink :: Maybe GuildId -> Maybe ChannelId -> Maybe MessageId -> String
getMessageLink (Just g) (Just c) (Just m) = "https://discord.com/channels/" ++ show g ++ "/" ++ show c ++ "/" ++ show m
getMessageLink _ _ _ = ""
