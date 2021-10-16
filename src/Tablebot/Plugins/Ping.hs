-- |
-- Module      : Tablebot.Plugins.Ping
-- Description : A very simple example plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which just responds "ping" to "!pong" and vice-versa.
module Tablebot.Plugins.Ping (pingPlugin) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.SmartCommand (parseComm)

-- | @ping@ is a command that takes no arguments (using 'noArguments') and
-- replies with "pong".
ping :: Command
ping =
  Command
    "ping"
    ( parseComm $ \m -> do
        _ <- sendMessage m "pong"
        return ()
    )

-- | @pong@ is a command that takes no arguments (using 'noArguments') and
-- replies with "ping". It is the younger sibling of @ping@.
pong :: Command
pong =
  Command
    "pong"
    ( parseComm $ \m -> do
        _ <- sendMessage m "ping"
        return ()
    )

pingHelp :: HelpPage
pingHelp = HelpPage "ping" "show a debug message" "**Ping**\nShows a debug message\n\n*Usage:* `ping`" [] None

pongHelp :: HelpPage
pongHelp = HelpPage "pong" "show a more different debug message" "**Pong**\nShows a different debug message\n\n*Usage:* `pong`" [] None

-- | @pingPlugin@ assembles these commands into a plugin containing both ping
-- and pong.
pingPlugin :: Plugin
pingPlugin = (plug "ping") {commands = [ping, pong], helpPages = [pingHelp, pongHelp]}
