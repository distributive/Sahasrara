module Tablebot.Plugin.Discord (
    sendMessage, sendMessageVoid, reactToCommand
) where

import Tablebot.Plugin (SD)

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Text
import Control.Monad.Trans.Class
import Control.Monad (void)

-- TODO: I am unsure what to do with the errors.
-- Maybe they should be called with fail, since SeldaT is a MonadFail?

sendMessage :: Message -> Text -> SD (Either RestCallErrorCode Message)
sendMessage m t = lift . restCall $ R.CreateMessage (messageChannel m) t

sendMessageVoid :: Message -> Text -> SD ()
sendMessageVoid m t = void $ sendMessage m t

reactToCommand :: Message -> Text -> SD (Either RestCallErrorCode ())
reactToCommand m e = lift . restCall $ R.CreateReaction (messageChannel m, messageId m) e