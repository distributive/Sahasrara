module Tablebot.Plugins.Ping (pingPlugin) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord
import Tablebot.Plugin.Parser (noArguments)

import Control.Monad.IO.Class

-- A very simple starter bot that responds to !ping.

ping :: Command b
ping = Command "ping" (noArguments $ \m -> do
    _ <- sendMessage m "pong"
    return ())

pong :: Command b
pong = Command "pong" (noArguments $ \m -> do
    _ <- sendMessage m "ping"
    return ())

pingPlugin :: Plugin b
pingPlugin = plug { commands = [ping, pong] }