-- |
-- Module      : Tablebot.Handler
-- Description : The event handler and cron runner for Tablebot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
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

import Control.Monad (unless)
import Control.Monad.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Discord.Types
import Tablebot.Handler.Command
  ( parseNewMessage,
  )
import Tablebot.Handler.Event
  ( parseMessageChange,
    parseOther,
    parseReactionAdd,
    parseReactionDel,
  )
import Tablebot.Handler.Plugins (changeAction)
import Tablebot.Handler.Types
import Tablebot.Plugin.Discord (sendEmbedMessage)
import Tablebot.Plugin.Exception
import UnliftIO.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import UnliftIO.Exception (catchAny)

-- | Given a combined plugin @pl@ and a command prefix @prefix@, builds an
-- event handler. This takes in each Discord 'Event' received (present in
-- "Discord.Types") and runs the relevant command or event handler from the
-- combined plugin.
eventHandler :: PluginActions -> Text -> Event -> CompiledDatabaseDiscord ()
eventHandler pl prefix = \case
  MessageCreate m ->
    ifNotBot m $ parseNewMessage pl prefix m `catch` \e -> changeAction () . sendEmbedMessage m "" $ embedError (e :: BotException)
  MessageUpdate cid mid ->
    parseMessageChange (compiledOnMessageChanges pl) True cid mid
  MessageDelete cid mid ->
    parseMessageChange (compiledOnMessageChanges pl) False cid mid
  MessageDeleteBulk cid mids ->
    mapM_ (parseMessageChange (compiledOnMessageChanges pl) False cid) mids
  MessageReactionAdd ri -> parseReactionAdd (compiledOnReactionAdds pl) ri
  MessageReactionRemove ri -> parseReactionDel (compiledOnReactionDeletes pl) ri
  -- TODO: MessageReactionRemoveAll is a bit of a pain as it gives us cid/mid,
  -- when we need ReactionInfo (contains a few extra bits).
  -- Similar with MessageReactionRemoveEmoji (removes all of one type).
  MessageReactionRemoveAll _cid _mid -> pure ()
  MessageReactionRemoveEmoji _rri -> pure ()
  e -> parseOther (compiledOtherEvents pl) e
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
runCron :: CompiledCronJob -> CompiledDatabaseDiscord ThreadId
runCron (CCronJob delay fn) = forkIO withDelay
  where
    withDelay :: CompiledDatabaseDiscord ()
    withDelay = do
      catchAny fn (liftIO . print)
      liftIO $ threadDelay delay
      withDelay

-- | @killCron@ takes a list of @ThreadId@ and kills each thread.
killCron :: [ThreadId] -> IO ()
killCron = mapM_ killThread
