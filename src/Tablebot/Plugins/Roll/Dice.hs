-- |
-- Module      : Tablebot.Plugins.Roll.Dice
-- Description : Lex, parse, and evaluate dice and other expressions using this plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the neccessary parsers and stucture to get the AST for an
-- expression that contains dice, as well as evaluate that expression.
module Tablebot.Plugins.Roll.Dice (eval, ListValues (..), defaultRoll, PrettyShow (prettyShow), basicFunctionsList, listFunctionsList, Converter (promote)) where

import Tablebot.Plugins.Roll.Dice.DiceData
  ( Converter (promote),
    Die (Die),
    Expr,
    ListValues (..),
    NumBase (Value),
  )
import Tablebot.Plugins.Roll.Dice.DiceEval (PrettyShow (prettyShow), eval)
import Tablebot.Plugins.Roll.Dice.DiceFunctions (basicFunctionsList, listFunctionsList)
import Tablebot.Plugins.Roll.Dice.DiceParsing ()

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
defaultRoll :: Expr
defaultRoll = promote (Die (Value 20))
