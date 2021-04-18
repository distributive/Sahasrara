module Plugin.Command (
    doCommand, toCommandFns
) where

import Plugin.Types

import Data.Text (Text)
import Discord.Types
import Data.Attoparsec.Text

toCommandFns :: [Plugin] -> [Parser (Message -> FeatureFn)]
toCommandFns = map parser . filter (\p -> case p of
    MessageChange _ -> True
    _ -> False) . concat

-- TODO: make this run each parser in turn and return the relevant FeatureFn.
-- Challenges: how to do errors effectively - we almost want the first part of the parser to decide which parser to use,
-- and then the second part of the parser can fail.
doCommand :: Text -> Message -> [Parser (Message -> FeatureFn)] -> FeatureFn
doCommand prefix m fs = undefined
    --case parse (string prefix *> choice fs) m of

