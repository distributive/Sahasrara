module Tablebot.Handler.Cache where

import Control.Concurrent.MVar (putMVar, readMVar, takeMVar)
import Control.Monad.Cont (join, liftM2)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Discord.Types
import Tablebot.Plugin.Types

lookupEmojiCache :: Text -> EnvDatabaseDiscord s (Maybe Emoji)
lookupEmojiCache t = do
  mcache <- liftCache ask
  cache <- liftIO $ readMVar mcache
  liftIO $ print $ M.lookup t $ cacheKnownEmoji cache
  pure $ M.lookup t $ cacheKnownEmoji cache

insertEmojiCache :: Text -> Emoji -> EnvDatabaseDiscord s ()
insertEmojiCache t e = do
  mcache <- liftCache ask
  cache <- liftIO $ takeMVar mcache
  let new = cache {cacheKnownEmoji = M.insert t e (cacheKnownEmoji cache)}
  liftIO $ putMVar mcache new

addNewEmojiCache :: Text -> Emoji -> EnvDatabaseDiscord s ()
addNewEmojiCache t e = do
  mcache <- liftCache ask
  cache <- liftIO $ takeMVar mcache
  let emap = cacheKnownEmoji cache
      new = cache {cacheKnownEmoji = if M.member t emap then emap else M.insert t e emap}
  liftIO $ putMVar mcache new

fillEmojiCache :: Guild -> EnvDatabaseDiscord s ()
fillEmojiCache guild = do
  let emoji = guildEmojis guild
  mapM_ (addNewEmojiCache =<< emojiName) emoji
