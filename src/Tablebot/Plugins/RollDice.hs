-- |
-- Module      : Tablebot.Plugins.RollDice
-- Description : A command that outputs the result of rolling dice.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs the result of rolling the input dice.
module Tablebot.Plugins.RollDice (rollPlugin) where

import Control.Monad.Writer (MonadIO (liftIO))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Discord.Types (Message)
import Tablebot.Plugin
import Tablebot.Plugin.Dice (Expr, defaultRoll, evalExpr, supportedFunctionsList)
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.SmartCommand (PComm (parseComm))
import Text.RawString.QQ (r)

rollDice' :: Maybe Expr -> Message -> DatabaseDiscord ()
rollDice' e' m = do
  let e = fromMaybe defaultRoll e'
  (v, s, _) <- liftIO $ evalExpr e
  sendMessage m $ pack $ "You rolled " ++ s ++ ".\nOutput: " ++ show v

rollDice :: Command
rollDice = Command "roll" (parseComm rollDice') []

rollDiceEquiv :: Command
rollDiceEquiv = Command "r" (parseComm rollDice') []

rollHelp :: HelpPage
rollHelp =
  HelpPage
    "roll"
    "roll dice and do maths"
    rollHelpText
    []
    None

rollHelpText :: Text
rollHelpText =
  pack $
    [r|**Roll**
Given an expression, evaluate the expression.

This supports addition, subtraction, multiplication, integer division, exponentiation, parentheses, dice of arbitrary size, dice with custom sides, rerolling dice once on a condition, rerolling dice indefinitely on a condition, keeping or dropping the highest or lowest dice, keeping or dropping dice based on a condition, and using functions like |]
      ++ intercalate ", " supportedFunctionsList
      ++ [r|.

To see a full list of uses and options, please go to <https://github.com/WarwickTabletop/tablebot/blob/main/docs/Roll.md>.

*Usage:*
  - `roll 1d20` -> rolls a twenty sided die and returns the outcome
  - `roll 3d6 + 5d4` -> sums the result of rolling three d6s and five d4s
  - `roll 2d20kh1` -> keeps the highest value out of rolling two d20s
  - `roll 5d10dl4` -> roll five d10s and drop the lowest four
|]

-- | @rollPlugin@ assembles the command into a plugin.
rollPlugin :: Plugin
rollPlugin = (plug "roll") {commands = [rollDice, rollDiceEquiv], helpPages = [rollHelp]}
