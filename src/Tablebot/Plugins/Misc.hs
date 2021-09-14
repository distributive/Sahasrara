-- |
-- Module      : Tablebot.Plugins.Misc
-- Description : A very simple example plugin.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which responds to vertain calls with specific responses.
module Tablebot.Plugins.Misc (miscPlugin) where

import Data.Text.Internal (Text)
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Parser (noArguments)
import Tablebot.Plugin.Types (Command (Command), Plugin (commands), plug)

miscCommands :: [(Text, Text)]
miscCommands =
  [ ("pr", "You can make a pull request for that!"),
    ("benji", ":benji_sit:")
  ]

-- | Given command text "a", reply with text "b".
baseCommand :: Text -> Text -> Command
baseCommand a b =
  Command
    a
    ( noArguments $ \m -> do
        _ <- sendMessage m b
        return ()
    )

-- | @miscPlugin@ assembles the call and response commands into a simple command list.
miscPlugin :: Plugin
miscPlugin = plug {commands = map (uncurry baseCommand) miscCommands}
