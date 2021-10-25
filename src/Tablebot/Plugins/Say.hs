-- |
-- Module      : Tablebot.Plugins.Say
-- Description : A command that outputs its input.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs its input.
module Tablebot.Plugins.Say (sayPlugin) where

import Data.Text (pack)
import Discord.Types (Message (messageAuthor), User (userId))
import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Parser (untilEnd)
import Text.RawString.QQ (r)

-- | @SS@ denotes the type returned by the command setup. Here its unused.
type SS = ()

-- | @say@ outputs its input.
say :: Command SS
say = Command "say" saycomm
  where
    saycomm :: Parser (Message -> DatabaseDiscord SS ())
    saycomm = do
      input <- untilEnd
      return $ \m -> do
        sendMessage m $ pack $ "> " ++ input ++ "\n - <@" ++ show (userId $ messageAuthor m) ++ ">"

sayHelp :: HelpPage
sayHelp =
  HelpPage
    "say"
    "make the bot speak"
    [r|**Say**
Repeat the input.

*Usage:* `say This text will be repeated by the bot!`|]
    []
    None

-- | @sayPlugin@ assembles the command into a plugin.
sayPlugin :: Plugin SS
sayPlugin = (plug "say") {commands = [say], helpPages = [sayHelp]}
