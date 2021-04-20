module Tablebot.Handler.Command (
    doCommand
) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessageVoid)
import Tablebot.Plugin.Parser (skipSpace)

import Data.Text (Text, stripPrefix, pack)
import Discord.Types
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error qualified as Err
import Control.Monad.IO.Class

doCommand :: Text -> Message -> [Command b] -> SeldaDiscord b ()
doCommand prefix m fs = let mt = messageText m in
    case stripPrefix prefix mt of
        Nothing -> pure ()
        Just text -> getCommand fs text m

-- Errors are reported in reverse order as the last one seems to be the user-facing one.

getCommand :: [Command b] -> Text -> Message -> SeldaDiscord b ()
getCommand [] _ m = sendMessageVoid m "Unknown command!"
getCommand (command:cs) mt m = let p = commandParser command
    in case stripPrefix (name command) mt of
        Nothing -> getCommand cs mt m
        Just text -> case parse (skipSpace *> p) "" text of
            Right p -> p m
            Left e -> case reverse $ map Err.messageString $ Err.errorMessages e of
                [] -> pure ()
                (x:_) -> sendMessageVoid m (pack x)