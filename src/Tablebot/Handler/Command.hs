module Tablebot.Handler.Command (
    parseCommands
) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessageVoid)
import Tablebot.Plugin.Parser (sp)

import Data.Text (Text, stripPrefix, pack, unpack)
import Discord.Types
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error qualified as Err
import Control.Monad.IO.Class

parseCommands :: [Command] -> Message -> Text -> DatabaseDiscord ()
parseCommands cs m prefix = case parse (parser cs) "" (messageText m) of
        Right p -> p m
        Left e -> case reverse $ map Err.messageString $ Err.errorMessages e of
            [] -> pure ()
            (x:_) -> sendMessageVoid m (pack x)
  where parser :: [Command] -> Parser (Message -> DatabaseDiscord ())
        parser cs = do
            string (unpack prefix)
            choice (map toErroringParser cs) <?> "No command with that name was found!"
          <|> pure (\_ -> pure ())
        toErroringParser :: Command -> Parser (Message -> DatabaseDiscord ())
        toErroringParser c = try (string (unpack $ name c)) *> sp *> commandParser c