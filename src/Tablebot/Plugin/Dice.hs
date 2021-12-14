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
module Tablebot.Plugin.Dice (evalListValues, ListValues (..), defaultRoll, PrettyShow (prettyShow), basicFunctionsList, Converter (promote)) where

-- module Tablebot.Plugin.Dice (evalExpr, Expr, PrettyShow (..), supportedFunctionsList, defaultRoll) where

-- module Tablebot.Plugin.Dice (evalExpr, Expr, PrettyShow (..), supportedFunctionsList, defaultRoll) where
import Tablebot.Plugin.Dice.DiceData
  ( Converter (promote),
    Die (Die),
    ListValues (..),
    NumBase (Value),
  )
import Tablebot.Plugin.Dice.DiceEval (PrettyShow (prettyShow), evalListValues)
import Tablebot.Plugin.Dice.DiceFunctions (basicFunctionsList)
import Tablebot.Plugin.Dice.DiceParsing ()

{-
if there is a gap between terms, any number of spaces (including none) is valid, barring in lstv, func, dice, die, dopr, ords; spaces are added manually in those.

lstv - expr | "{" spcs expr ("," spcs expr spcs)* spcs "}" | nbse "#" base | funcBasics
expr - term ([+-] expr)?
term - nega ([*/] term)?
nega - "-" expo | expo
expo - func "^" expo | func
func - funcBasics | base
base - dice | nbse
nbse - "(" expr ")" | [0-9]+
dice - base die dopr?
die  - "d" "!"? (bse | "{" spcs expr (spcs ", " spcs expr)* spcs "}")
dopr - "!"? (("rr" | "ro") ords | ("k"|"d") (("l" | "h") nbse | "w" ords))
ords - ("/=" | "<=" | ">=" | "<" | "=" | ">") nbase
spcs - " "*
funcBasics - {some string identifier} "(" spcs (lstv (spcs "," spcs lstv)*)? spcs ")"
-}

-- | The default expression to evaluate if no expression is given.
defaultRoll :: ListValues
defaultRoll = promote (Die (Value 20))
