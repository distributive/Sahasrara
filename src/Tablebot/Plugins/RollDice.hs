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
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, pack, replicate, unpack)
import Discord.Types (Message (messageAuthor))
import Tablebot.Plugin
import Tablebot.Plugin.Dice
import Tablebot.Plugin.Dice.DiceData
import Tablebot.Plugin.Discord (Format (Code), formatInput, sendMessage, toMention)
import Tablebot.Plugin.Parser (inlineCommandHelper)
import Tablebot.Plugin.SmartCommand (PComm (parseComm), Quoted (Qu), pars)
import Text.Megaparsec (MonadParsec (try), choice, (<?>))
import Text.RawString.QQ (r)

rollDice' :: Maybe ListValues -> Maybe (Quoted Text) -> Message -> DatabaseDiscord ()
rollDice' e' t m = do
  let e = fromMaybe defaultRoll e'
  (vs, ss) <- liftIO $ evalListValues e
  sendMessage m (makeMsg vs ss e)
  where
    dsc = maybe ": " (\(Qu t') -> " \"" <> t' <> "\": ") t
    baseMsg = toMention (messageAuthor m) <> " rolled" <> dsc
    makeLine (i, s) = pack (formatInput Code $ show i) <> Data.Text.replicate (max 0 (6 - length (show i))) " " <> " ⟵ " <> s
    makeMsg [] _ _ = baseMsg <> "No output."
    makeMsg [v] [s] _ = baseMsg <> s <> ".\nOutput: " <> pack (show v)
    makeMsg [v] _ e = baseMsg <> formatInput Code (prettyShow e) <> ".\nOutput: " <> pack (show v)
    makeMsg vs [s] _ = baseMsg <> s <> ".\nOutput: " <> pack (show vs)
    makeMsg vs ss e = baseMsg <> formatInput Code (prettyShow e) <> "\n  " <> intercalate "\n  " (makeLine <$> zip vs ss)

rollDiceParser :: Parser (Message -> DatabaseDiscord ())
rollDiceParser = choice (try <$> options)
  where
    options =
      [ parseComm (\lv -> rollDice' (Just lv) Nothing),
        parseComm (rollDice' Nothing Nothing),
        try (parseComm (\lv qt -> rollDice' (Just lv) (Just qt))) <?> "",
        try (parseComm (rollDice' Nothing . Just)) <?> ""
      ]

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
Given an expression, evaluate the expression. Can roll inline using |]
      ++ "`[|to roll|]`."
      ++ [r|

This supports addition, subtraction, multiplication, integer division, exponentiation, parentheses, dice of arbitrary size, dice with custom sides, rerolling dice once on a condition, rerolling dice indefinitely on a condition, keeping or dropping the highest or lowest dice, keeping or dropping dice based on a condition, and using functions like |]
      ++ unpack (intercalate ", " supportedFunctionsList)
      ++ [r|.

To see a full list of uses and options, please go to <https://github.com/WarwickTabletop/tablebot/blob/main/docs/Roll.md>.

*Usage:*
  - `roll 1d20` -> rolls a twenty sided die and returns the outcome
  - `roll 3d6 + 5d4` -> sums the result of rolling three d6s and five d4s
  - `roll 2d20kh1` -> keeps the highest value out of rolling two d20s
  - `roll 5d10dl4` -> roll five d10s and drop the lowest four
|]

genchar :: Command
genchar = Command "genchar" (snd $ head rpgSystems') (toCommand <$> rpgSystems')
  where
    doDiceRoll (nm, lv) = (nm, parseComm $ rollDice' (Just lv) (Just (Qu ("genchar for " <> nm))))
    rpgSystems' = doDiceRoll <$> rpgSystems
    toCommand (nm, ps) = Command nm ps []

rpgSystems :: [(Text, ListValues)]
rpgSystems =
  [ ("dnd", MultipleValues (Value 6) (DiceBase (Dice (NBase (Value 6)) (Die (Value 6)) (Just (DieOpRecur (DieOpOptionKD Drop (Low (Value 1))) Nothing))))),
    ("wfrp", MultipleValues (Value 8) (NBase (Paren (Add (promote (Value 20)) (promote (Die (Value 10)))))))
  ]

gencharHelp :: HelpPage
gencharHelp =
  HelpPage
    "genchar"
    "generate stat arrays for some systems"
    ("**Genchar**\nCan be used to generate stat arrays for certain systems.\n\nCurrently supported systems: " <> intercalate ", " (fst <$> rpgSystems) <> ".\n\n*Usage:* `genchar`, `genchar dnd`")
    []
    None

-- | @rollPlugin@ assembles the command into a plugin.
rollPlugin :: Plugin
rollPlugin =
  (plug "roll")
    { commands = [rollDice, commandAlias "r" rollDice, genchar],
      helpPages = [rollHelp, gencharHelp],
      inlineCommands = [rollDiceInline]
    }
