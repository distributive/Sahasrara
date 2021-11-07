-- |
-- Module      : Tablebot.Handler.Command
-- Description : The event handler for received messages.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module deals with 'Command's and 'InlineCommand's, checking for the
-- command prefix in the case of 'Command's and then trying each plugin-defined
-- parser to see if it matches.
module Tablebot.Handler.Command
  ( parseNewMessage,
    parseCommands,
    parseInlineCommands,
  )
where

import Data.Text (Text, isPrefixOf)
import Discord.Types (Message (messageText))
import Tablebot.Handler.Plugins (changeAction)
import Tablebot.Handler.Types
import Tablebot.Plugin.Discord (sendEmbedMessage)
import Tablebot.Plugin.Exception (BotException (ParserException), embedError)
import Tablebot.Plugin.Parser (skipSpace1)
import Tablebot.Plugin.Types hiding (commandParser, inlineCommandParser)
import Text.Megaparsec

-- | @parseNewMessage@ parses a new message, first by attempting to match the
-- bot's prefix to the start of the message, then (if that fails) by attempting
-- to find inline commands.
parseNewMessage :: PluginActions -> Text -> Message -> CompiledDatabaseDiscord ()
parseNewMessage pl prefix m =
  if prefix `isPrefixOf` messageText m
    then parseCommands (compiledCommands pl) m prefix
    else parseInlineCommands (compiledInlineCommands pl) m

-- | Given a list of 'Command' @cs@, the 'Message' that triggered the event
-- @m@, and a command prefix @prefix@, construct a parser that parses commands.
-- We look for the prefix, followed by trying out the name of each command,
-- then on finding a valid command name running that command's parser to get a
-- runnable function @Message -> DatabaseDiscord ()@ which is then run.
--
-- If the parser errors, the last error (which is hopefully one created by
-- '<?>') is sent to the user as a Discord message.
parseCommands :: [CompiledCommand] -> Message -> Text -> CompiledDatabaseDiscord ()
parseCommands cs m prefix = case parse (parser cs) "" (messageText m) of
  Right p -> p m
  Left e -> changeAction () . sendEmbedMessage m "" $ embedError $ ParserException $ "```\n" ++ errorBundlePretty e ++ "```"
  where
    parser :: [CompiledCommand] -> Parser (Message -> CompiledDatabaseDiscord ())
    parser cs' =
      do
        _ <- chunk prefix
        choice (map toErroringParser cs') <?> "No command with that name was found!"
        <|> pure (\_ -> pure ())
    toErroringParser :: CompiledCommand -> Parser (Message -> CompiledDatabaseDiscord ())
    toErroringParser c = try (chunk $ commandName c) *> (skipSpace1 <|> eof) *> commandParser c

-- | Given a list of 'InlineCommand' @cs@ and a message @m@, run each inline
-- command's parser on the message text until one succeeds. Errors are not sent
-- to the user, and do not halt command attempts (achieved using 'try').
parseInlineCommands :: [CompiledInlineCommand] -> Message -> CompiledDatabaseDiscord ()
parseInlineCommands cs m = case parse (parser cs) "" (messageText m) of
  Right p -> p m
  Left _ -> pure ()
  where
    parser :: [CompiledInlineCommand] -> Parser (Message -> CompiledDatabaseDiscord ())
    parser cs' = choice $ map (try . inlineCommandParser) cs'
