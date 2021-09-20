{-|
Module      : Tablebot.Plugins.Ping
Description : A very simple example plugin.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

A command that picks one random element from its given arguments.
-}
module Tablebot.Plugins.Flip (flipPlugin) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser
import Tablebot.Util.Random

import Prelude hiding (flip)

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Text.Megaparsec

-- | @flip@ picks one of its arguments at random, or one of "heads" and "tails"
-- if none are provided.
flip :: Command
flip = Command "flip" flipcomm
    where
        flipcomm :: Parser (Message -> DatabaseDiscord ())
        flipcomm = do
            args <- nonSpaceWord `sepBy` space
            return $ \m -> do
                choice <- case length args of
                    0         -> liftIO $ chooseOne ["Heads", "Tails"]
                    1         -> pure "You only specified one item!"
                    otherwise -> liftIO $ chooseOne args
                sendMessage m $ pack choice
                return ()

-- | @flipPlugin@ as     sembles the command into a plugin.
flipPlugin :: Plugin
flipPlugin = plug { commands = [flip] }
