module Tablebot.Plugin.Help where

import Data.Functor (void, ($>))
import Data.Text (Text)
import qualified Data.Text as T
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser (skipSpace)
import Tablebot.Plugin.Types
import Text.Megaparsec (choice, chunk, eof, try, (<?>), (<|>))

rootBody :: Text
rootBody =
  "**Tabletop Bot**\n\
  \This friendly little bot provides several tools to help with\
  \ the running of the Warwick Tabletop Games and Role-Playing Society Discord server."

generateHelp :: Plugin -> Plugin
generateHelp p =
  p
    { commands = Command "help" (handleHelp (helpPages p)) : commands p
    }

handleHelp :: [HelpPage] -> Parser (Message -> DatabaseDiscord ())
handleHelp hp = parseHelpPage root
  where
    root = HelpPage "" "" rootBody hp

parseHelpPage :: HelpPage -> Parser (Message -> DatabaseDiscord ())
parseHelpPage hp = do
  chunk (helpName hp)
  skipSpace
  (try eof $> displayHelp hp) <|> choice (map parseHelpPage $ helpSubpages hp) <?> "Unknown Subcommand"

displayHelp :: HelpPage -> Message -> DatabaseDiscord ()
displayHelp hp m = void $ sendMessage m $ formatHelp hp

formatHelp :: HelpPage -> Text
formatHelp hp = helpBody hp <> formatSubpages hp
  where
    formatSubpages :: HelpPage -> Text
    formatSubpages (HelpPage _ _ _ []) = ""
    formatSubpages hp' = "\n\n*Subcommands*" <> T.concat (map formatSubpage (helpSubpages hp'))
    formatSubpage :: HelpPage -> Text
    formatSubpage hp' = "\n`" <> helpName hp' <> "` " <> helpShortText hp'
