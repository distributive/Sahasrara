-- |
-- Module      : Tablebot.Plugins.Flip
-- Description : A very simple example plugin.
-- Copyright   : (c) Amelie WD 2021
-- License     : MIT
-- Maintainer  : tablebot@ameliewd.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that picks one random element from its given arguments.
module Tablebot.Plugins.Flip (flipPlugin) where

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser
import Tablebot.Util.Random
import Text.Megaparsec
import Prelude hiding (flip)

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
          0 -> liftIO $ chooseOneWithDefault "" ["Heads", "Tails"]
          1 -> pure "You only specified one item!"
          otherwise -> liftIO $ chooseOneWithDefault (head args) args
        sendMessage m $ pack choice
        return ()

flipHelp :: HelpPage
flipHelp = HelpPage "flip" "flip a coin, or randomly pick from a given list" "**Flip**\nRandomly picks one element from its arguments or, if none are provided, picks from heads and tails. \n\n*Usage:*\n`flip`\n`flip first second third`" []

-- | @flipPlugin@ assembles the command into a plugin.
flipPlugin :: Plugin
flipPlugin = plug {commands = [flip], helpPages = [flipHelp]}
