module Sahasrara.Handler
  ( eventHandler,
    runCron,
    killCron,
    submitApplicationCommands,
  )
where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Monad (unless, void)
import Control.Monad.Exception (MonadException (catch))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Bifunctor (Bifunctor (second))
import Data.Map as M (fromList)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.Sqlite (SqlBackend, runSqlPool)
import Discord (Cache (cacheApplication), DiscordHandler, readCache, restCall)
import Discord.Interactions (ApplicationCommand (..), Interaction (..))
import Discord.Requests (ChannelRequest (JoinThread))
import Discord.Types
import Sahasrara.Internal.Handler.Command (parseNewMessage)
import Sahasrara.Internal.Handler.Event
  ( parseApplicationCommandRecv,
    parseComponentRecv,
    parseMessageChange,
    parseOther,
    parseReactionAdd,
    parseReactionDel,
  )
import Sahasrara.Internal.Plugins (changeAction)
import Sahasrara.Internal.Types
import Sahasrara.Utility.Discord (createApplicationCommand, interactionResponseCustomMessage, removeApplicationCommandsNotInList, sendChannelEmbedMessage)
import Sahasrara.Utility.Exception (BotException, embedError)
import Sahasrara.Utility.Types (MessageDetails (messageDetailsEmbeds), SahasraraCache (cacheApplicationCommands), liftDiscord, messageDetailsBasic)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
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
    ifNotBot m $ catchErrors (messageChannelId m) $ parseNewMessage pl prefix m
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
  InteractionCreate i@InteractionComponent {} -> parseComponentRecv (compiledOnComponentRecvs pl) i `interactionErrorCatch` i
  InteractionCreate i@InteractionApplicationCommand {} -> parseApplicationCommandRecv i `interactionErrorCatch` i
  InteractionCreate i@InteractionApplicationCommandAutocomplete {} -> parseApplicationCommandRecv i `interactionErrorCatch` i
  -- TODO: add application command autocomplete as an option
  ThreadCreate c -> changeAction () $ void $ liftDiscord $ restCall $ JoinThread (channelId c)
  e -> parseOther (compiledOtherEvents pl) e
  where
    ifNotBot m = unless (userIsBot (messageAuthor m))
    interactionErrorCatch action i = action `catch` (\e -> changeAction () . interactionResponseCustomMessage i $ (messageDetailsBasic "") {messageDetailsEmbeds = Just [embedError (e :: BotException)]})
    catchErrors m = (`catch` (\e -> changeAction () . sendChannelEmbedMessage m "" $ embedError (e :: BotException)))

-- | @runCron@ takes an individual @CronJob@ and runs it in a separate thread.
-- The @ThreadId@ is returned so it can be killed later.
-- The @CronJob@ itself consists of a @delay@ in /microseconds/ (see
-- 'Control.Concurrent.threadDelay') and a computation @fn@ to run repeatedly,
-- each separated by a delay of @delay@.
--
-- Due to how @runSqlPool@ runs a transaction when started, we need it to be
-- run with each call of a @CronJob@ - so we pass it in as argument and
-- manually unwrap the monad transformer stack within each call.
runCron ::
  Pool SqlBackend ->
  CompiledCronJob ->
  ReaderT (MVar SahasraraCache) DiscordHandler ThreadId
runCron pool (CCronJob delay fn) = do
  cache <- ask
  lift . forkIO $ withDelay cache
  where
    withDelay :: MVar SahasraraCache -> DiscordHandler ()
    withDelay cache = do
      catchAny (runSqlPool (runReaderT fn cache) pool) (liftIO . print)
      liftIO $ threadDelay delay
      withDelay cache

-- | @killCron@ takes a list of @ThreadId@ and kills each thread.
killCron :: [ThreadId] -> IO ()
killCron = mapM_ killThread

-- | Given a list of compiled application commands and a pointer to the
-- Sahasrara cache, create the given application commands, purge ones that
-- weren't created by us, and place the application command id's and their
-- actions in the cache.
submitApplicationCommands :: [CompiledApplicationCommand] -> MVar SahasraraCache -> DiscordHandler ()
submitApplicationCommands compiledAppComms cacheMVar =
  ( do
      -- generate the application commands, cleaning up any application commands we don't like
      serverIdStr' <- liftIO $ lookupEnv "SERVER_ID"
      case serverIdStr' of
        Nothing -> pure ()
        Just serverIdStr -> do
          serverId <- readServerStr serverIdStr
          aid <- partialApplicationID . cacheApplication <$> readCache
          applicationCommands <-
            mapM
              ( \(CApplicationCommand cac action) -> do
                  ac <- createApplicationCommand aid serverId cac
                  return (applicationCommandId ac, action)
              )
              compiledAppComms
          removeApplicationCommandsNotInList aid serverId (fst <$> applicationCommands)
          liftIO $ takeMVar cacheMVar >>= \tcache -> putMVar cacheMVar $ tcache {cacheApplicationCommands = M.fromList (second (lift .) <$> applicationCommands)}
  )
    `catch` \(e :: IOError) -> liftIO $ putStrLn $ "There was an error of some sort when submitting the application commands - verify that `SERVER_ID` is set properly. (" <> show e <> ")"
  where
    readServerStr :: String -> DiscordHandler (Maybe GuildId)
    readServerStr "global" = return Nothing
    readServerStr s = maybe (fail $ "could not read server id: " <> show s) (return . Just) (readMaybe s)
