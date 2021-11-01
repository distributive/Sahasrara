-- |
-- Module      : Tablebot.Plugin.Discord
-- Description : Discord helpers for building plugins.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains helpful Discord functionality for building plugins
-- without having to lift Discord operations constantly.
module Tablebot.Plugin.Discord
  ( sendMessage,
    sendEmbedMessage,
    reactToMessage,
    getMessage,
    getMessageMember,
    Message,
  )
where

import Control.Monad.Exception
import Data.Text (Text)
import Discord (RestCallErrorCode, restCall)
import qualified Discord.Requests as R
import Discord.Types
import Tablebot.Handler.Embed
import Tablebot.Plugin (EnvDatabaseDiscord, liftDiscord)
import Tablebot.Plugin.Exception (BotException (..))

-- | @sendMessage@ sends the input message @t@ in the same channel as message
-- @m@. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
sendMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s ()
sendMessage m t = do
  res <- liftDiscord . restCall $ R.CreateMessage (messageChannel m) t
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

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
  EnvDatabaseDiscord s ()
sendEmbedMessage m t e = do
  res <- liftDiscord . restCall $ TablebotEmbedRequest (messageChannel m) t (asEmbed e)
  case res of
    Left _ -> throw $ MessageSendException "Failed to send embed message."
    Right _ -> return ()

-- | @getMessage@ gets the relevant 'Message' object for a given 'ChannelId'
-- and 'MessageId', or returns an error ('RestCallErrorCode').
getMessage ::
  ChannelId ->
  MessageId ->
  EnvDatabaseDiscord s (Either RestCallErrorCode Message)
getMessage cid mid = liftDiscord . restCall $ R.GetChannelMessage (cid, mid)

-- | @reactToMessage@ reacts to the given message with the emoji specified
-- by the text input (see README.md from discord-haskell). Returns @()@ if
-- successful, else the error ('RestCallErrorCode').
reactToMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s (Either RestCallErrorCode ())
reactToMessage m e =
  liftDiscord . restCall $
    R.CreateReaction (messageChannel m, messageId m) e

-- | @getMessageMember@ returns the message member object if it was sent from a Discord server,
-- or @Nothing@ if it was sent from a DM (or the API fails)
getMessageMember :: Message -> EnvDatabaseDiscord s (Maybe GuildMember)
getMessageMember m = gMM (messageGuild m) m
  where
    maybeRight :: Either a b -> Maybe b
    maybeRight (Left _) = Nothing
    maybeRight (Right a) = Just a
    gMM :: Maybe GuildId -> Message -> EnvDatabaseDiscord s (Maybe GuildMember)
    gMM Nothing _ = return Nothing
    gMM (Just g') m' = do
      a <- liftDiscord $ restCall $ R.GetGuildMember g' (userId $ messageAuthor m')
      return $ maybeRight a
