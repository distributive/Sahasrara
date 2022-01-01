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

import Data.Text (Text)
import Tablebot.Utility
import Tablebot.Utility.Discord (Message, sendMessage)
import Tablebot.Utility.SmartParser (parseComm)

-- | @echo@ pulled out to help resolve parser overlapping instances errors.
-- Sends the provided text, regardless of received message.
echo :: Text -> Message -> DatabaseDiscord ()
echo t m = sendMessage m t

-- | @ping@ is a command that takes no arguments (using 'noArguments') and
-- replies with "pong".
ping :: Command
ping =
  Command
    "ping"
    ( parseComm $ echo "pong"
    )
    []

-- | @pong@ is a command that takes no arguments (using 'noArguments') and
-- replies with "ping". It is the younger sibling of @ping@.
pong :: Command
pong =
  Command
    "pong"
    ( parseComm $ echo "ping"
    )
    []

pingHelp :: HelpPage
pingHelp = HelpPage "ping" [] "show a debug message" "**Ping**\nShows a debug message\n\n*Usage:* `ping`" [] None

pongHelp :: HelpPage
pongHelp = HelpPage "pong" [] "show a more different debug message" "**Pong**\nShows a different debug message\n\n*Usage:* `pong`" [] None

-- | @pingPlugin@ assembles these commands into a plugin containing both ping
-- and pong.
pingPlugin :: Plugin
pingPlugin = (plug "ping") {commands = [ping, pong], helpPages = [pingHelp, pongHelp]}
