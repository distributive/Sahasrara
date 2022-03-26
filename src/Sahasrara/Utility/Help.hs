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

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Sahasrara.Internal.Permission (getSenderPermission, userHasPermission)
import Sahasrara.Internal.Plugins (changeAction)
import Sahasrara.Internal.Types
import Sahasrara.Utility.Discord (Message, sendMessage)
import Sahasrara.Utility.Parser (skipSpace)
import Sahasrara.Utility.Permission (requirePermission)
import Sahasrara.Utility.Types hiding (helpPages)
import Text.Megaparsec (choice, chunk, eof, try, (<?>), (<|>))

rootBody :: Text
rootBody =
  "**Sahasrara**\n\
  \A Discord bot interface for Netrunner cards and more."

helpHelpPage :: HelpPage
helpHelpPage = HelpPage "help" [] "show information about commands" "**Help**\nShows information about bot commands\n\n*Usage:* `help <page>`" [] None

generateHelp :: CombinedPlugin -> CombinedPlugin
generateHelp p =
  p
    { combinedSetupAction = return (PA [CCommand "help" (handleHelp (helpHelpPage : combinedHelpPages p)) []] [] [] [] [] [] []) : combinedSetupAction p
    }

handleHelp :: [HelpPage] -> Parser (Message -> CompiledDatabaseDiscord ())
handleHelp hp = parseHelpPage root
  where
    root = HelpPage "" [] "" rootBody hp None

parseHelpPage :: HelpPage -> Parser (Message -> CompiledDatabaseDiscord ())
parseHelpPage hp = do
  _ <- choice (map chunk (helpName hp : helpAliases hp))
  skipSpace
  (try eof $> displayHelp hp) <|> choice (map parseHelpPage $ helpSubpages hp) <?> "Unknown Subcommand"

displayHelp :: HelpPage -> Message -> CompiledDatabaseDiscord ()
displayHelp hp m = changeAction () . requirePermission (helpPermission hp) m $ do
  uPerm <- getSenderPermission m
  sendMessage m $ formatHelp uPerm hp

formatHelp :: UserPermission -> HelpPage -> Text
formatHelp up hp = helpBody hp <> formatSubpages hp
  where
    formatSubpages :: HelpPage -> Text
    formatSubpages (HelpPage _ _ _ _ [] _) = ""
    formatSubpages hp' = if T.null sp then "" else "\n\n*Subcommands*" <> sp
      where
        sp = T.concat (map formatSubpage (helpSubpages hp'))
    formatSubpage :: HelpPage -> Text
    formatSubpage hp' = if userHasPermission (helpPermission hp') up then "\n`" <> helpName hp' <> "` " <> helpShortText hp' else ""
