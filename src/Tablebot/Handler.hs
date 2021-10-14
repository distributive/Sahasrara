-- |
-- Module      : Tablebot.Handler
-- Description : The event handler and cron runner for Tablebot.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides most of the functionality in "Tablebot". This
-- includes rerouting the various Discord events to the right kinds of commands,
-- running cron jobs at Discord startup and killing those jobs after completion.
module Tablebot.Handler
  ( eventHandler,
    runCron,
    killCron,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Text (Text)
import Discord.Types
import Tablebot.Handler.Command
  ( parseCommands,
    parseInlineCommands,
  )
import Tablebot.Handler.Event
  ( parseMessageChange,
    parseOther,
    parseReactionAdd,
    parseReactionDel,
  )
import Tablebot.Plugin

import Control.Monad.Exception
import Tablebot.Plugin.Error
import Tablebot.Plugin.Discord (sendEmbedMessageVoid)

-- | Given a combined plugin @pl@ and a command prefix @prefix@, builds an
-- event handler. This takes in each Discord 'Event' received (present in
-- "Discord.Types") and runs the relevant command or event handler from the
-- combined plugin.
eventHandler :: Plugin -> Text -> Event -> DatabaseDiscord ()
eventHandler pl prefix = \case
  MessageCreate m -> ifNotBot m $ do
    parseCommands (commands pl) m prefix `catch` \e -> sendEmbedMessageVoid m "" $ embedError $ (e :: BotException)
    parseInlineCommands (inlineCommands pl) m
  MessageUpdate cid mid ->
    parseMessageChange (onMessageChanges pl) True cid mid
  MessageDelete cid mid ->
    parseMessageChange (onMessageChanges pl) False cid mid
  MessageDeleteBulk cid mids ->
    mapM_ (parseMessageChange (onMessageChanges pl) False cid) mids
  MessageReactionAdd ri -> parseReactionAdd (onReactionAdds pl) ri
  MessageReactionRemove ri -> parseReactionDel (onReactionDeletes pl) ri
  -- TODO: MessageReactionRemoveAll is a bit of a pain as it gives us cid/mid,
  -- when we need ReactionInfo (contains a few extra bits).
  -- Similar with MessageReactionRemoveEmoji (removes all of one type).
  MessageReactionRemoveAll _cid _mid -> pure ()
  MessageReactionRemoveEmoji _rri -> pure ()
  e -> parseOther (otherEvents pl) e
  where
    ifNotBot m = unless (userIsBot (messageAuthor m))

-- | @runCron@ takes an individual @CronJob@ and runs it in a separate thread.
-- The @ThreadId@ is returned so it can be killed later.
-- The @CronJob@ itself consists of a @delay@ in /microseconds/ (see
-- 'Control.Concurrent.threadDelay') and a computation @fn@ to run repeatedly,
-- each separated by a delay of @delay@.
--
-- This is implemented by removing the @ReaderT@ layers of the
-- @DatabaseDiscord@ monad transformer stack and then running a lifted @forkIO@
-- so may need rewriting if you change the @DatabaseDiscord@ monad stack.
runCron :: CronJob -> DatabaseDiscord ThreadId
runCron (CronJob delay fn) = do
  db <- ask
  discord <- lift ask
  let unDB = runReaderT fn db
  let unDiscord = runReaderT unDB discord
  liftIO $ forkIO (loopWithDelay delay unDiscord)
  where
    loopWithDelay :: Int -> IO () -> IO ()
    loopWithDelay del fn' = fn' >> threadDelay del >> loopWithDelay del fn'

-- | @killCron@ takes a list of @ThreadId@ and kills each thread.
killCron :: [ThreadId] -> IO ()
killCron = mapM_ killThread
