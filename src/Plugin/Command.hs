module Plugin.Command (
    doCommand, toCommandFns
) where

import Plugin.Types

import Data.Text (Text, stripPrefix)
import Discord.Types
import Data.Attoparsec.Text
import Control.Monad.IO.Class
import Control.Monad
import Debug.Trace

toCommandFns :: [Plugin b] -> [(Text, Parser (Message -> FH b))]
toCommandFns = map (\p -> (name p, commandParser p))
    . filter (\case
    Command _ _ -> True
    _ -> False) . concat

doCommand :: Text -> Message -> [(Text, Parser (Message -> FH b))] -> FH b
doCommand prefix m fs = let mt = messageText m in
    case stripPrefix prefix mt of
        Nothing -> pure ()
        Just text -> getCommand fs text m

getCommand :: [(Text, Parser (Message -> FH b))] -> Text -> Message -> FH b
getCommand [] _ _ = liftIO $ putStrLn "no command found"
getCommand ((n, p):ps) mt m = traceShow n $
    case stripPrefix n mt of
        Nothing -> getCommand ps mt m
        Just text -> case parseOnly (skipSpace *> p) text of
            Right p -> p m
            Left e -> liftIO $ putStrLn $ "error: " ++ e