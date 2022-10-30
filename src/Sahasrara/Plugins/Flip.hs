-- |
-- Module      : Sahasrara.Plugins.Flip
-- Description : A command that flips a coin, or randomly selects from a list.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that picks one random element from its given arguments.
module Sahasrara.Plugins.Flip (flipPlugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text, pack)
import Data.Maybe (catMaybes, fromMaybe)
import Discord.Types
import Sahasrara.Utility
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (formatFromEmojiName, sendEmbedMessage, sendEmbedInteractionWithButtons, basicButton)
import Sahasrara.Utility.Embed (addColour, basicEmbed)
import Sahasrara.Utility.Parser
import Sahasrara.Utility.Random (chooseOne)
import Sahasrara.Utility.SmartParser -- (Labelled (..), SenderUserId, PComm (parseComm), makeApplicationCommandPair)
import Text.Megaparsec
import Text.RawString.QQ
import Prelude hiding (flip)
import Discord.Interactions (Interaction (..))
import Sahasrara.Internal.Handler.Command (parseValue)

-- | @flip@ picks one of its arguments at random, or one of "Corp" and "Runner"
-- if none are provided.
flipComm :: Maybe Text -> UserId -> DatabaseDiscord MessageDetails
flipComm arg uid = do
  -- args <- parseValue flipPars $ fromMaybe "" arg
  let args = []
  c <- case length args of
    0 -> liftIO $ chooseOne [":arrow_up: Heads", ":arrow_down: Tails"]
    _ -> liftIO $ chooseOne $ map ("> " <>) args
  sendEmbedInteractionWithButtons "" [button] $ basicEmbed ":coin: Result :coin:" c
  where
    flipPars :: Parser [Text]
    flipPars = do
      args <- (try quoted <|> nonSpaceWord) `sepBy` some space
      return $ map pack args
    button :: Button
    button = basicButton "Reroll" "🔄" "flip" "reroll" uid

flipSlashCommand :: Labelled "args" "strings to select from" (Maybe Text) -> SenderUserId -> DatabaseDiscord MessageDetails
flipSlashCommand (Labelled arg) (SenderUserId uid) = flipComm arg uid

rerollFlipRecv :: ComponentRecv
rerollFlipRecv = ComponentRecv "reroll" (processComponentInteraction' rerollParser True)
  where
    rerollParser :: Parser (Interaction -> DatabaseDiscord MessageDetails)
    rerollParser = onlyAllowRequestor $ \arg -> flipComm arg

flipHelp :: HelpPage
flipHelp =
  HelpPage
    "flip"
    []
    "randomly picks either 'Heads' or 'Tails', or from a given list"
    [r|Randomly picks one element from its arguments or, if none are provided, either 'Heads' or 'Tails'.

**Usage**
`flip` outputs 'Heads' or 'Tails' with uniform probability
`flip a b c` outputs 'a', 'b', or 'c' with uniform probability|]
    []
    None

side :: Command
side = Command "side" (parseComm sideComm) []
  where
    sideComm :: () -> Message -> DatabaseDiscord ()
    sideComm () m = do
      corp <- formatFromEmojiName "s_corp"
      runner <- formatFromEmojiName "s_runner"
      result <- liftIO $ chooseOne [corp <> " Corp", runner <> " Runner"]
      sendEmbedMessage m "" $ basicEmbed ":coin: Result :coin:" result

sideHelp :: HelpPage
sideHelp =
  HelpPage
    "side"
    []
    "picks a Netrunner side at random"
    [r|Randomly picks 'Corp' or 'Runner' with uniform probability.

**Usage**
`side` outputs 'Corp' or 'Runner' with uniform probability|]
    []
    None

mark :: Command
mark = Command "mark" (parseComm markComm) []
  where
    markComm :: () -> Message -> DatabaseDiscord ()
    markComm () m = do
      (result, colour) <- liftIO $ chooseOne [("HQ", colHQ), ("R&D", colRnD), ("Archives", colArchives)]
      sendEmbedMessage m "" $ addColour colour $ basicEmbed ":game_die: Result :game_die:" ("**Your mark is:** " <> result)

markHelp :: HelpPage
markHelp =
  HelpPage
    "mark"
    []
    "identifies your mark"
    [r|Randomly picks one of the three central servers with uniform probability.

**Usage**
`mark` outputs 'HQ', 'R&D', or 'Archives' with uniform probability|]
    []
    None

-- | @flipPlugin@ assembles the command into a plugin.
flipPlugin :: Plugin
flipPlugin =
  (plug "flip")
  {
    commands = [side, mark],
    applicationCommands = catMaybes [makeApplicationCommandPair "flip" "choose one of several strings" flipSlashCommand],
    onComponentRecvs = [rerollFlipRecv],
    helpPages = [flipHelp, sideHelp, markHelp]
  }
