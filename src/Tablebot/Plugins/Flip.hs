-- |
-- Module      : Tablebot.Plugins.Flip
-- Description : A command that flips a coin, or randomly selects from a list.
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
import Tablebot.Plugin.Discord (Message, sendMessageVoid)
import Tablebot.Plugin.Parser
import Tablebot.Util.Random
import Text.Megaparsec
import Text.RawString.QQ
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
          otherwise -> liftIO $ chooseOneWithDefault (head args) args
        sendMessageVoid m $ pack choice

flipHelp :: HelpPage
flipHelp =
  HelpPage
    "flip"
    "flip a coin, or randomly pick from a given list"
    [r|**Flip**
Randomly picks one element from its arguments or, if none are provided, picks from heads and tails.

*Usage:*
`flip`
`flip first second third`|]
    []
    None

-- | @flipPlugin@ assembles the command into a plugin.
flipPlugin :: Plugin
flipPlugin = plug {commands = [flip], helpPages = [flipHelp]}
