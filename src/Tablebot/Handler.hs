module Tablebot.Handler (
    eventHandler, runCron, killCron
) where

import Tablebot.Plugin
import Tablebot.Plugin.Types (DatabaseDiscord)
import Tablebot.Handler.Command

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
        MessageCreate m -> unless (userIsBot (messageAuthor m)) $
            parseCommands (commands pl) m prefix
        _ -> pure ()

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