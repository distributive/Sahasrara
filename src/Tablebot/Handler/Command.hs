module Tablebot.Handler.Command (
    doCommand
) where

import Tablebot.Plugin

import Data.Text (Text, stripPrefix)
import Discord.Types
import Data.Attoparsec.Text
import Control.Monad.IO.Class
import Control.Monad

doCommand :: Text -> Message -> [Command b] -> SeldaDiscord b
doCommand prefix m fs = let mt = messageText m in
    case stripPrefix prefix mt of
        Nothing -> pure ()
        Just text -> getCommand fs text m

getCommand :: [Command b] -> Text -> Message -> SeldaDiscord b
getCommand [] _ _ = liftIO $ putStrLn "no command found"
getCommand (command:cs) mt m = let p = commandParser command
    in case stripPrefix (name command) mt of
        Nothing -> getCommand cs mt m
        Just text -> case parseOnly (skipSpace *> p) text of
            Right p -> p m
            Left e -> liftIO $ putStrLn $ "error: " ++ e