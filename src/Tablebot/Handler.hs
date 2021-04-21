module Tablebot.Handler (
    eventHandler, runCron, killCron
) where

import Tablebot.Plugin
import Tablebot.Plugin.Types (DatabaseDiscord)
import Tablebot.Handler.Command
import Tablebot.Handler.Event

import Discord
import Discord.Types
import Data.Text (Text)
import Control.Monad (unless)
import Control.Concurrent
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

eventHandler :: Plugin -> Text -> Event -> DatabaseDiscord ()
eventHandler pl prefix = \case
        MessageCreate m -> ifNotBot m $ do
            parseCommands (commands pl) m prefix
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
        MessageReactionRemoveAll cid mid -> pure ()
        MessageReactionRemoveEmoji rri -> pure ()
        e -> parseOther (otherEvents pl) e
    where ifNotBot m = unless (userIsBot (messageAuthor m))

-- forkIO only works on direct IO, so we have to unwrap and rewrap the stack
runCron :: CronJob -> DatabaseDiscord ThreadId
runCron (CronJob delay fn) = do
    db <- ask
    discord <- lift ask
    let unDB = runReaderT fn db
    let unDiscord = runReaderT unDB discord
    liftIO $ forkIO (loopWithDelay delay unDiscord)
    where loopWithDelay :: Int -> IO () -> IO ()
          loopWithDelay del fn = fn >> threadDelay del >> loopWithDelay del fn

killCron :: [ThreadId] -> IO ()
killCron = mapM_ killThread