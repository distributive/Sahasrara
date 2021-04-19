module Features.Ping where

import Plugin.Plugin

import Control.Monad.IO.Class

-- A very simple starter bot that responds to !ping.

ping :: Feature b
ping = Command "ping" (pure $ \_ -> liftIO $ putStrLn "pong")

pingPlugin :: Plugin b
pingPlugin = [ping]