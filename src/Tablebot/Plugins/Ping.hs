module Tablebot.Plugins.Ping (pingPlugin) where

import Tablebot.Plugin

import Control.Monad.IO.Class

-- A very simple starter bot that responds to !ping.

ping :: Command b
ping = Command "ping" (pure $ \_ -> liftIO $ putStrLn "pong")

pingPlugin :: Plugin b
pingPlugin = plug { commands = [ping] }