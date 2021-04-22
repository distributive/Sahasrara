{-|
Module      : Tablebot.Plugins.Ping
Description : A very simple example plugin.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

This is an example plugin which just responds "ping" to "!pong" and vice-versa.
-}
module Tablebot.Plugins.Ping (pingPlugin) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Parser (noArguments)

-- | @ping@ is a command that takes no arguments (using 'noArguments') and
-- replies with "pong".
ping :: Command
ping = Command "ping" (noArguments $ \m -> do
    _ <- sendMessage m "pong"
    return ())

-- | @pong@ is a command that takes no arguments (using 'noArguments') and
-- replies with "ping". It is the younger sibling of @ping@.
pong :: Command
pong = Command "pong" (noArguments $ \m -> do
    _ <- sendMessage m "ping"
    return ())

-- | @pingPlugin@ assembles these commands into a plugin containing both ping
-- and pong.
pingPlugin :: Plugin
pingPlugin = plug { commands = [ping, pong] }