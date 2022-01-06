-- |
-- Module      : Tablebot.Plugins.Suggest
-- Description : A plugin to show links to suggest a new game to buy
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that shows the link for a user to suggest a new game to buy.
module Tablebot.Plugins.Suggest (suggestPlugin) where

import Data.Text (pack)
import Tablebot.Utility
import Tablebot.Utility.Discord (Message, sendMessage)
import Tablebot.Utility.Parser
import Text.Megaparsec

-- | @suggest@ is a command that returns a link to the website where users can
-- suggest a new game to buy, depending on the game category they want to
-- suggest.
suggest :: Command
suggest = Command "suggest" suggestcomm []
  where
    suggestcomm :: Parser (Message -> DatabaseDiscord ())
    suggestcomm = do
      args <- nonSpaceWord `sepBy` space
      return $ \m -> do
        c <- case args of
          ["boardgame"] -> return "Want to suggest a board game? Here you go! -> https://www.warwicktabletop.co.uk/inventory/boardgames/suggestions/"
          ["ccg"] -> return "Want to suggest a CCG? Here you go! -> https://www.warwicktabletop.co.uk/inventory/ccgs/suggestions/"
          ["wargame"] -> return "Want to suggest a wargame? Here you go! -> https://www.warwicktabletop.co.uk/inventory/wargames/suggestions/"
          ["rpg"] -> return "Want to suggest a RPG? Here you go! -> https://www.warwicktabletop.co.uk/inventory/rpgs/suggestions/"
          _ -> return "Invalid input! Please try again with one of the following: `boardgame`, `ccg`, `wargame`, or `rpg`"
        sendMessage m $ pack c

suggestHelp :: HelpPage
suggestHelp = HelpPage "suggest" [] "show links to suggest a new game for the society to buy" "**Suggest**\nShows the link to suggest a new game for the society to buy\n\n*Usage:* \n- `suggest boardgame` -> get the link to suggest a new boardgame to buy\n- `suggest ccg` -> get the link to suggest a new CCG to buy\n- `suggest wargame` -> get the link to suggest a new wargame to buy\n- `suggest rpg` -> get the link to request a new RPG to buy" [] None

suggestPlugin :: Plugin
suggestPlugin = (plug "suggest") {commands = [suggest], helpPages = [suggestHelp]}
