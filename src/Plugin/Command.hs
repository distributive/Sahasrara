module Plugin.Command (
    doCommand, toCommandFns
) where

import Plugin.Types

import Data.Text (Text)
import Discord.Types
import Data.Attoparsec.Text
import Control.Monad.Trans.Class

toCommandFns :: [Plugin] -> [Parser (Message -> FeatureFn)]
toCommandFns = map (\p -> string (name p) *> parser p)
    . filter (\pl -> case pl of
    MessageChange _ -> True
    _ -> False) . concat

doCommand :: Text -> Message -> [Parser (Message -> FeatureFn)] -> FeatureFn
doCommand prefix m fs = 
    case parseOnly (string prefix *> choice fs) (messageText m) of
        Right p -> p m
        Left e -> lift $ putStrLn $ "error: " ++ e