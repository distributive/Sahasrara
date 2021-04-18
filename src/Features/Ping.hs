module Features.Ping where

import Plugin.Plugin

import Control.Monad.Trans.Class

-- A very simple starter bot that responds to !ping.

ping :: Feature
ping = Command "ping" (pure $ \_ -> lift $ putStrLn "pong")

pingPlugin :: Plugin
pingPlugin = [ping]