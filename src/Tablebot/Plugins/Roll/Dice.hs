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
--
-- The behind the scenes for the dice is split into four files.
-- - DiceData - the data structures for the AST for dice
-- - DiceFunctions - functionality for dealing with functions and processing
--    them
-- - DiceParsing - parsers for getting all the DiceData items
-- - DiceEval - methods for evaluating elements from DiceData
--
-- Below is the regex representing the parsing for the expressions, and
-- explanations for each component
--
-- If there is a gap between terms, any number of spaces (including none) is valid, barring in lstv, dice, die, dopr, ords; spaces are added manually in those.
--
-- lstv - nbse "#" base | funcBasics | lstb
-- lstb - "{" expr ("," expr)* "}" | "(" lstv ")"
-- expr - term ([+-] expr)?
-- term - nega ([*/] term)?
-- nega - "-" expo | expo
-- expo - func "^" expo | func
-- func - funcBasics | base
-- base - dice | nbse
-- nbse - "(" expr ")" | [0-9]+
-- dice - base die dopr?
-- die  - "d" "!"? (bse | lstb)
-- dopr - dopo+
-- dopo - "!"? (("rr" | "ro") ords | ("k"|"d") (("l" | "h") nbse | "w" ords))
-- ords - ("/=" | "<=" | ">=" | "<" | "=" | ">") nbase
-- spcs - " "*
-- argv - lstv | expr
-- funcBasics - {some string identifier} "(" (argv ("," argv)*)? ")"
--
-- lstv (ListValues)                   - representing all possible list values (basic list values, functions that return lists, and values which are lists of length N that consist of `Base`s)
-- lstb (ListValuesBase)               - representing some basic list values (those that can be used in dice expressions, such as manually created lists and bracketed `ListValues`)
-- expr (Expr)                         - representing addition, subtraction, or a single `Term` value
-- term (Term)                         - representing multiplication, division, or a single `Negation` value
-- nega (Negation)                     - representing a negation, or a single `Expo` value
-- expo (Expo)                         - representing exponentiation or a single `Func` value
-- func (Func)                         - representing a function that returns an integer, or a single `Base` value
-- base (Base)                         - representing a base value, which is either a dice value or a `NumBase` value
-- nbse (NumBase)                      - representing an integer or an expression in parentheses
-- dice (Dice)                         - representing an amount of dice of a given size (a `Base`) and type (dependent on what the `Die` is), with certain modifiers optionally given (in the `DieOpRecur`)
-- die  (Die)                          - representing a die with either a `NumBase` or a `ListValuesBase` to choose a value from. The die could also be a lazy die, where the option is evaluated repeatedly
-- dopr (DieOpRecur)                   - representing one or more `DieOpOption`s
-- dopo (DieOpOption)                  - representing one of rerolling on a condition, keeping/dropping certain values (the highest/lowest `NumBase` values or where values meet a condition), or performing another operation lazily (meaning that the values should be evaluated every time they need)
-- ords (AdvancedOrdering and NumBase) - representing a more complex ordering operation than a basic `Ordering`, when compared to a `NumBase`
-- argv (ArgValue)                     - representing an argument to a function
-- funcBasics                          - a generic regex representation for a general function parser
module Tablebot.Plugins.Roll.Dice (evalInteger, evalList, ListValues (..), defaultRoll, PrettyShow (prettyShow), integerFunctionsList, listFunctionsList, Converter (promote)) where

import Tablebot.Plugins.Roll.Dice.DiceData
  ( Converter (promote),
    Die (Die),
    Expr,
    ListValues (..),
    NumBase (Value),
  )
import Tablebot.Plugins.Roll.Dice.DiceEval (PrettyShow (prettyShow), evalInteger, evalList)
import Tablebot.Plugins.Roll.Dice.DiceFunctions (integerFunctionsList, listFunctionsList)
import Tablebot.Plugins.Roll.Dice.DiceParsing ()

-- | The default expression to evaluate if no expression is given.
defaultRoll :: Expr
defaultRoll = promote (Die (Value 20))
