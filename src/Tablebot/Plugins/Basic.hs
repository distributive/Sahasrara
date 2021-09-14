-- |
-- Module      : Tablebot.Plugins.Basic
-- Description : A very simple example plugin.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which responds to vertain calls with specific responses.
module Tablebot.Plugins.Basic (basicPlugin) where

import Data.Text.Internal (Text)
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Parser (noArguments)
import Tablebot.Plugin.Types (Command (Command), Plugin (commands), plug)

-- | The basic commands. Put the command in the first element of each tuple, and the 
-- response in the second element of each tuple.
basicCommands :: [(Text, Text)]
basicCommands =
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

-- | @basicPlugin@ assembles the call and response commands into a simple command list.
basicPlugin :: Plugin
basicPlugin = plug {commands = map (uncurry baseCommand) basicCommands}
