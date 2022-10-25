-- -- |
-- Module      : Sahasrara.Utility.Help
-- Description : Help text generation and storage
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This module creates functions and data structures to help generate help text for commands
module Sahasrara.Utility.Help where

import Data.Default (Default (def))
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Types
import Sahasrara.Internal.Permission (getSenderPermission, userHasPermission)
import Sahasrara.Internal.Plugins (changeAction)
import Sahasrara.Internal.Types
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (sendEmbedMessage)
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Parser (skipSpace)
import Sahasrara.Utility.Permission (requirePermission)
import Sahasrara.Utility.Types hiding (helpPages)
import Text.Megaparsec (choice, chunk, eof, try, (<?>), (<|>))
import Text.RawString.QQ (r, rQ)

rootBody :: Text
rootBody =
  [rQ|A Discord Netrunner bot.

**Searching for [Netrunner](https://netrunnerdb.com) cards**
`[[card]]` to view a card
`{{card}}` to view its art
`<<card>>` to view its flavour text
`((card))` to view its legality history

**Additional parameters**
`[[card|set]]` to view the printing of a card from a named set
`[[card|n]]` to view the nth printing of a card (0 is the first, -1 is the last)

**Searching for [Original Netrunner](https://www.emergencyshutdown.net/webminster) cards**
`[|card|~]` to view an ONR card
`{|card|}` to view its art
`<|card|>` to view its flavour text|]

helpHelpPage :: HelpPage
helpHelpPage =
  HelpPage
    "help"
    []
    "shows information about a specific command"
    [r|Shows information about bot commands.

**Usage**
`help` shows the general information about this bot
`help <command>` shows the documentation for the given command|]
    []
    None

generateHelp :: Text -> CombinedPlugin -> CombinedPlugin
generateHelp rootText p =
  p
    { combinedSetupAction = return (def {compiledCommands = [CCommand "help" (handleHelp rootText (helpHelpPage : combinedHelpPages p)) []]}) : combinedSetupAction p
    }

-- Assumes only the root page will have an empty name
isHelpPageRoot :: HelpPage -> Bool
isHelpPageRoot hp = helpName hp == ""

handleHelp :: Text -> [HelpPage] -> Parser (Message -> CompiledDatabaseDiscord ())
handleHelp rootText hp = parseHelpPage root
  where
    root = HelpPage "" [] "" rootText hp None

parseHelpPage :: HelpPage -> Parser (Message -> CompiledDatabaseDiscord ())
parseHelpPage hp = do
  _ <- choice (map chunk (helpName hp : helpAliases hp))
  skipSpace
  (try eof $> displayHelp hp) <|> choice (map parseHelpPage $ helpSubpages hp) <?> "Unknown Subcommand"

displayHelp :: HelpPage -> Message -> CompiledDatabaseDiscord ()
displayHelp hp m = changeAction () . requirePermission (helpPermission hp) m $ do
  uPerm <- getSenderPermission m
  sendEmbedMessage m "" $ addColour colHelp $ CreateEmbed "" "" Nothing (formatHelpTitle hp) "" Nothing (formatHelp uPerm hp) [] Nothing "" Nothing Nothing Nothing

formatHelpTitle :: HelpPage -> Text
formatHelpTitle hp = ":scroll:  " <> if isHelpPageRoot hp then "Sahasrara" else "Help: `$" <> helpName hp <> "`"

formatHelp :: UserPermission -> HelpPage -> Text
formatHelp up hp = helpBody hp <> formatSubpages hp
  where
    formatSubpages :: HelpPage -> Text
    formatSubpages (HelpPage _ _ _ _ [] _) = ""
    formatSubpages hp' = if T.null sp then "" else header <> sp
      where
        header = if isHelpPageRoot hp' then "\n\n**Commands**" else "\n\n*Subcommands*"
        sp = T.concat (map formatSubpage (helpSubpages hp'))
    formatSubpage :: HelpPage -> Text
    formatSubpage hp' = if userHasPermission (helpPermission hp') up then "\n`" <> helpName hp' <> "` " <> helpShortText hp' else ""
