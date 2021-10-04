-- -- |
-- Module      : Tablebot.Plugins.Basic
-- Description : A very simple example plugin.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which responds to certain calls with specific responses.
module Tablebot.Plugins.Basic (basicPlugin) where

import Data.Text.Internal (Text)
import Data.Text (toTitle)
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Parser (noArguments)
import Tablebot.Plugin.Types (Command (Command), Plugin (commands), plug, HelpPage(HelpPage), helpPages)

-- * Some types to help clarify what's going on
-- | @MiniHelpPage@ simplifies creating the help pages. You can either provide just the short and long help text and let 
-- it autogenerate the formatting, or you can provide a full help page if you want more control
data MiniHelpPage = Simple (Text, Text) | Advanced HelpPage
-- | @BasicCommand@ is a tuple containing the important for this command.
-- Put the command in the first element, the response in the second element of each tuple, 
-- and a 'MiniHelpPage' for your command in the third
type BasicCommand = (Text, Text, MiniHelpPage)

-- | The basic commands.
basicCommands :: [BasicCommand]
basicCommands =
  [ ("pr", "You can make a pull request for that!", 
      Simple ("you know what to do", "You know what to do")),
    ("benji", ":benji_sit:", 
      Simple ("the almost mascot", "Though he may sit, when put to test, the gender cube proved it was best"))
  ]

-- | Given command text "a", reply with text "b".
baseCommand :: BasicCommand -> Command
baseCommand (a, b, _) =
  Command
    a
    ( noArguments $ \m -> do
        _ <- sendMessage m b
        return ()
    )

baseHelp :: BasicCommand -> HelpPage
baseHelp (_, _, Advanced help) = help
baseHelp (a, _, Simple (short, long)) = HelpPage a short ("**"<>toTitle a<>"**\n"<>long<>"\n\n*Usage:* `"<>a<>"`") []


-- | @basicPlugin@ assembles the call and response commands into a simple command list.
basicPlugin :: Plugin
basicPlugin = plug {commands = map baseCommand basicCommands,
                    helpPages = map baseHelp basicCommands}
