{-|
Module      : Tablebot.Plugins.Say
Description : A very simple example plugin.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

A command that outputs its input.
-}
module Tablebot.Plugins.Say (sayPlugin) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessageVoid)
import Tablebot.Plugin.Parser (untilEnd)

import Data.Text (Text, pack)
import Text.Megaparsec

-- | @say@ outputs its input.
say :: Command
say = Command "say" saycomm
    where
        saycomm :: Parser (Message -> DatabaseDiscord ())
        saycomm = do
            input <- untilEnd <?> ""
            return $ \m -> do
                sendMessageVoid m $ pack $ input ++ " - " 

-- | @sayPlugin@ assembles the command into a plugin.
sayPlugin :: Plugin
sayPlugin = plug { commands = [say] }
