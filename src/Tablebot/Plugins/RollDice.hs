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
import Data.Text (Text, pack, replicate, unpack)
import Discord.Types (Message (messageAuthor))
import Tablebot.Plugin
import Tablebot.Plugin.Dice
import Tablebot.Plugin.Discord (sendMessage, toMention)
import Tablebot.Plugin.Parser (inlineCommandHelper, skipSpace1)
import Tablebot.Plugin.SmartCommand (Quoted (Qu), pars)
import Text.Megaparsec (MonadParsec (eof), (<|>))
import Text.RawString.QQ (r)

rollDice' :: Maybe ListValues -> Maybe (Quoted Text) -> Message -> DatabaseDiscord ()
rollDice' e' t m = do
  let e = fromMaybe defaultRoll e'
  -- (v, s) <- liftIO $ evalExpr e
  (vs, ss) <- liftIO $ evalListValues e
  -- let msg = toMention (messageAuthor m) <> " rolled" <> dsc <> s <> ".\nOutput: " <> pack (show v)
  sendMessage m (makeMsg vs ss e)
  where
    dsc = maybe ": " (\(Qu t') -> " \"" <> t' <> "\": ") t
    baseMsg = toMention (messageAuthor m) <> " rolled" <> dsc
    makeLine (i,s) = unpack (pack ("`"++show i++"`") <> Data.Text.replicate (max 0 (6- length (show i)))  " " <> " ⟵ " <> s)
    makeMsg [] _ _ = baseMsg <> "No output."
    makeMsg [v] [s] _ = baseMsg <> s <> ".\nOutput: " <> pack (show v)
    makeMsg [v] _  e= baseMsg <> pack (prettyShow e) <> ".\nOutput: " <> pack (show v)
    makeMsg vs [s] _ = baseMsg <> s <> ".\nOutput: " <> pack (show vs)
    makeMsg vs ss _ = baseMsg <> "\n  " <> pack (intercalate "\n  " (makeLine <$> zip vs ss))

rollDiceParser :: Parser (Message -> DatabaseDiscord ())
rollDiceParser = do
  e <- pars @(Maybe ListValues)
  maybe (return ()) (const (skipSpace1 <|> eof)) e
  t <- pars @(Maybe (Quoted Text))
  return $ rollDice' e t

rollDice :: Command
rollDice = Command "roll" rollDiceParser []

rollDiceInline :: InlineCommand
rollDiceInline = inlineCommandHelper "[|" "|]" pars (\e m -> rollDice' (Just e) Nothing m)

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
rollPlugin =
  (plug "roll")
    { commands = [rollDice, commandAlias "r" rollDice],
      helpPages = [rollHelp],
      inlineCommands = [rollDiceInline]
    }
