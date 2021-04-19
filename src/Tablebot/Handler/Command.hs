module Tablebot.Handler.Command (
    doCommand
) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessageVoid)

import Data.Text (Text, stripPrefix, pack)
import Discord.Types
import Data.Attoparsec.Text
import Control.Monad.IO.Class

doCommand :: Text -> Message -> [Command b] -> SeldaDiscord b ()
doCommand prefix m fs = let mt = messageText m in
    case stripPrefix prefix mt of
        Nothing -> pure ()
        Just text -> getCommand fs text m

-- TODO: Error messages are kind of ugly because they're followed by the parser name.
-- Apparently Parsec does this better - move to Parsec.

getCommand :: [Command b] -> Text -> Message -> SeldaDiscord b ()
getCommand [] _ m = sendMessageVoid m "Unknown command!"
getCommand (command:cs) mt m = let p = commandParser command
    in case stripPrefix (name command) mt of
        Nothing -> getCommand cs mt m
        Just text -> case parseOnly (skipSpace *> p) text of
            Right p -> p m
            Left e -> sendMessageVoid m (pack e)