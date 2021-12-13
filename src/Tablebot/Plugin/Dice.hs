-- |
-- Module      : Tablebot.Plugin.Dice
-- Description : Lex, parse, and evaluate dice and other expressions using this plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the neccessary parsers and stucture to get the AST for an
-- expression that contains dice, as well as evaluate that expression.
module Tablebot.Plugin.Dice (evalListValues, ListValues (..), defaultRoll, PrettyShow (prettyShow), supportedFunctionsList) where

-- module Tablebot.Plugin.Dice (evalExpr, Expr, PrettyShow (..), supportedFunctionsList, defaultRoll) where

import Tablebot.Plugin.Dice.DiceData
  ( Base (DiceBase, NBase),
    Dice (Dice),
    Die (Die),
    Expo (NoExpo),
    Expr (NoExpr),
    Func (NoFunc),
    ListValues (..),
    Negation (NoNeg),
    NumBase (Value),
    Term (NoTerm),
  )
import Tablebot.Plugin.Dice.DiceEval (PrettyShow (prettyShow), evalListValues)
import Tablebot.Plugin.Dice.DiceFunctions (supportedFunctionsList)
import Tablebot.Plugin.Dice.DiceParsing ()

{-
if there is a gap between terms, any number of spaces (including none) is valid, barring in die, dopr, and ords

expr - term ([+-] expr)?
term - func ([*/] term)?
func - {some string identifier}? " " nega
nega - "-" expo | expo
expo - base "^" expo | base
base - dice | nbse
nbse - "(" expr ")" | [0-9]+
dice - base die dopr?
die  - "d" "!"? (bse | "{" expr (", " expr)* "}")
dopr - "!"? (("rr" | "ro") ords | ("k"|"d") (("l" | "h") nbse | "w" ords))
ords - ("/=" | "<=" | ">=" | "<" | "=" | ">") nbase
-}

-- | The default expression to evaluate if no expression is given.
defaultRoll :: ListValues
defaultRoll = NoList $ NoExpr $ NoTerm $ NoNeg $ NoExpo $ NoFunc $ DiceBase $ Dice (NBase (Value 1)) (Die (Value 20)) Nothing

-- TODO: full check over of bounds. make this thing AIR TIGHT.
