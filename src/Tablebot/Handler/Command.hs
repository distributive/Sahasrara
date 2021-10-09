-- |
-- Module      : Tablebot.Handler.Command
-- Description : The event handler for received messages.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module deals with 'Command's and 'InlineCommand's, checking for the
-- command prefix in the case of 'Command's and then trying each plugin-defined
-- parser to see if it matches.
module Tablebot.Handler.Command
  ( parseCommands,
    parseInlineCommands,
  )
where

import Data.Text (Text, pack, stripPrefix, unpack)
import Discord.Types (Message (messageText))
import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessageVoid)
import Tablebot.Plugin.Parser (sp)
import Text.Megaparsec

-- | Given a list of 'Command' @cs@, the 'Message' that triggered the event
-- @m@, and a command prefix @prefix@, construct a parser that parses commands.
-- We look for the prefix, followed by trying out the name of each command,
-- then on finding a valid command name running that command's parser to get a
-- runnable function @Message -> DatabaseDiscord ()@ which is then run.
--
-- If the parser errors, the last error (which is hopefully one created by
-- '<?>') is sent to the user as a Discord message.
parseCommands :: [Command] -> Message -> Text -> DatabaseDiscord ()
parseCommands cs m prefix = case parse (parser cs) "" (messageText m) of
  Right p -> p m
  Left e -> sendMessageVoid m . pack $ "```\n" ++ errorBundlePretty e ++ "```"
  where
    parser :: [Command] -> Parser (Message -> DatabaseDiscord ())
    parser cs =
      do
        chunk prefix
        choice (map toErroringParser cs) <?> "No command with that name was found!"
        <|> pure (\_ -> pure ())
    toErroringParser :: Command -> Parser (Message -> DatabaseDiscord ())
    toErroringParser c = try (chunk $ name c) *> sp *> commandParser c

-- | Given a list of 'InlineCommand' @cs@ and a message @m@, run each inline
-- command's parser on the message text until one succeeds. Errors are not sent
-- to the user, and do not halt command attempts (achieved using 'try').
parseInlineCommands :: [InlineCommand] -> Message -> DatabaseDiscord ()
parseInlineCommands cs m = case parse (parser cs) "" (messageText m) of
  Right p -> p m
  Left e -> pure ()
  where
    parser :: [InlineCommand] -> Parser (Message -> DatabaseDiscord ())
    parser cs = choice $ map (try . inlineCommandParser) cs
