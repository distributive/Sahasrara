-- -- |
-- Module      : Sahasrara.Plugins.Basic
-- Description : A very simple example plugin.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which responds to certain calls with specific responses.
module Sahasrara.Plugins.Basic (basicPlugin) where

import Data.Text as T (Text, toTitle)
import Discord.Internal.Rest (Message)
import Sahasrara.Utility.Discord (sendMessage)
import Sahasrara.Utility.SmartParser (parseComm)
import Sahasrara.Utility.Types
  ( Command,
    DatabaseDiscord,
    EnvCommand (Command),
    EnvInlineCommand (InlineCommand),
    EnvPlugin (commands, inlineCommands),
    HelpPage (HelpPage),
    InlineCommand,
    Plugin,
    RequiredPermission (None),
    helpPages,
    plug,
  )
import Text.Megaparsec (anySingle, skipManyTill)
import Text.Megaparsec.Char (string')

-- | @MiniHelpPage@ simplifies creating the help pages. You can either provide just the short and long help text and let
-- it autogenerate the formatting, or you can provide a full help page if you want more control
data MiniHelpPage = Simple (Text, Text) | Advanced HelpPage

-- | @BasicCommand@ is a tuple containing the important information for this command.
-- Put the command in the first element, the response in the second element of each tuple,
-- and a 'MiniHelpPage' for your command in the third
type BasicCommand = (Text, Text, MiniHelpPage)

-- | The basic commands.
basicCommands :: [BasicCommand]
basicCommands = []
  -- [ ( "ping",
  --     "Ping!",
  --     Simple ("pings this bot", "Pings this bot")
  --   ),
  --   ( "pong",
  --     "Pong!",
  --     Simple ("pongs this bot", "Pongs this bot")
  --   )
  -- ]

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
baseHelp (a, _, Simple (short, long)) = HelpPage a [] short ("**" <> toTitle a <> "**\n" <> long <> "\n\n*Usage:* `" <> a <> "`") [] None

type BasicInlineCommand = (Text, Text)

basicInlineCommands :: [BasicInlineCommand]
basicInlineCommands = [] -- Example: [("thank you", "You're welcome!")]

baseInlineCommand :: BasicInlineCommand -> InlineCommand
baseInlineCommand (t, rs) = InlineCommand (skipManyTill anySingle (string' t) >> return (`sendMessage` rs))

-- | @basicPlugin@ assembles the call and response commands into a simple command list.
basicPlugin :: Plugin
basicPlugin =
  (plug "basic")
    { commands = map baseCommand basicCommands,
      helpPages = map baseHelp basicCommands,
      inlineCommands = map baseInlineCommand basicInlineCommands
    }
