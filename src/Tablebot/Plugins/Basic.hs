-- -- |
-- Module      : Tablebot.Plugins.Basic
-- Description : A very simple example plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which responds to certain calls with specific responses.
module Tablebot.Plugins.Basic (basicPlugin) where

import Data.Text (toTitle)
import Data.Text.Internal (Text)
import Tablebot.Plugin (DatabaseDiscord)
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.SmartCommand (parseComm)
import Tablebot.Plugin.Types
  ( Command,
    EnvCommand (Command),
    EnvPlugin (commands),
    HelpPage (HelpPage),
    Plugin,
    RequiredPermission (None),
    helpPages,
    plug,
  )

-- * Some types to help clarify what's going on

-- | @MiniHelpPage@ simplifies creating the help pages. You can either provide just the short and long help text and let
-- it autogenerate the formatting, or you can provide a full help page if you want more control
data MiniHelpPage = Simple (Text, Text) | Advanced HelpPage

-- | @BasicCommand@ is a tuple containing the important information for this command.
-- Put the command in the first element, the response in the second element of each tuple,
-- and a 'MiniHelpPage' for your command in the third
type BasicCommand = (Text, Text, MiniHelpPage)

-- | The basic commands.
basicCommands :: [BasicCommand]
basicCommands =
  [ ( "pr",
      "You can make a pull request for that!",
      Simple ("you know what to do", "You know what to do")
    ),
    ( "issue",
      "You can submit an issue for that!",
      Simple ("you know what you want someone else to do", "You know what you want someone else to do")
    ),
    ( "benji",
      "<:benji_sit:686131468560367627>",
      Simple ("the almost mascot", "Though he may sit, when put to test, the gender cube proved it was best")
    ),
    ( "about",
      "This bot was created by finnbar to replace a couple of other bots in Tabletop. It's written in Haskell, and you can find the github here: <https://github.com/WarwickTabletop/tablebot>. There are setup guides and a contributor's guide to help you get started.",
      Simple ("you know what you want someone else to do", "You know what you want someone else to do")
    )
  ]

-- | @echo@ pulled out to help resolve parser overlapping instances errors.
-- Sends the provided text, regardless of received message.
echo :: Text -> Message -> DatabaseDiscord ()
echo t m = sendMessage m t

-- | Given command text "a", reply with text "b".
baseCommand :: BasicCommand -> Command
baseCommand (a, b, _) =
  Command
    a
    (parseComm $ echo b)
    []

baseHelp :: BasicCommand -> HelpPage
baseHelp (_, _, Advanced help) = help
baseHelp (a, _, Simple (short, long)) = HelpPage a short ("**" <> toTitle a <> "**\n" <> long <> "\n\n*Usage:* `" <> a <> "`") [] None

-- | @basicPlugin@ assembles the call and response commands into a simple command list.
basicPlugin :: Plugin
basicPlugin =
  (plug "basic")
    { commands = map baseCommand basicCommands,
      helpPages = map baseHelp basicCommands
    }
