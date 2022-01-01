# Roll Command

The roll command has a staggering amount of flexibility, as well as additional features.

Below are listed the current full capabilities of the bot for rolling dice and evaluation expressions. All operations (currently) result in integers or a list. A list of functions is available in the [functions section](#Functions).

## Basic Operators

- Addition
- Subtraction
- Multiplication
- Division
- Negation using `-`
- Exponentiation
- Parentheses

## Dice Operations

As well as the arithmetic operators above, dice can be rolled, hence the name of the command.

The basic format for this is `dX` where X is some number, meaning a single die of size X. Multiple dice can be rolled using `YdX`, meaning that Y dice are rolled of size X. Parentheses can be used for both Y and X in this case. If Y is greater than a number determined by the bot owner (150 by default), the roll will not be executed. This is the same number that governs the total amount of RNG calls allowed within a command's execution.

In addition to the above, there is syntax for rolling dice with arbitrary sides - `d{4,7,19,-5}`. This results in a die that is equally likely to result in four, seven, nineteen, or minus five. These numbers could be any expression instead.

There is support for stacking dice. This means that if you write `2d4d5d6`, it will be parsed and executed as `((2d4)d5)d6`. Operations can be applied to the dice in this stack.

When the bot returns the result, it also shows the results of the die rolls of the outer most dice. If values are bolded, they are "criticals" (they are the maximum or minimum value of the die; as such, they only work on standard dice). If values are crossed out (strike through) the die has been rerolled from that value and the next value it had is to the right (see below for reroll syntax). If the value is underlined, the die was dropped at some stage (see below for drop/keep syntax).

### Manipulating Dice

There are operators that can be performed on dice rolling, listed below. Wherever there is `X`, it means an integer (or an expression within parentheses; if the integer is negative it is instead treated as 0). Wherever there is a `#`, it means one of `/=` (not equals), `<=`, `>=`, `<`, `=`, `>`. There can be multiple operators, and operators are applied left to right, so if all but one die is dropped in the first step, that die is the only die that operations will be performed on in later steps.

- `khX` - keep highest X
- `klX` - keep lowest X
- `dhX` - drop highest X
- `dlX` - drop lowest X
- `kw#X` - keep where the condition relative to X is kept
- `dw#X` - drop where the condition relative to X is kept
- `ro#X` - reroll once where the condition relative to X is kept
- `rr#X` - reroll indefinitely where the condition relative to X is kept

These operations can be stuck on the end of a dice value; see below for examples.

- `2d20kh1` - keep the highest roll of rolling two twenty sided dice
- `10d6ro<3` - roll ten six sided dice, rerolling any that are less than three once
- `4d10dw=10rr<3` - roll four ten sided dice, dropping any that equal ten, then rerolling any left that are less than 3

### Evaluating Lazily

There is additional notation for evaluating dice in different ways, detailed below. They use `!` to denote the special evaluation being used. It is called "evaluating the expression lazily" because the values used are evaluated when they are asked for, not precalculated and then stored.

- `5d!(2d6)rr<4` - every time this die is rolled, the size of the die is randomly determined by evaluating `2d6`. If the value rolled is less than 4, the die is rerolled
- `6d!{1d6,7+1d6}` - every time this die is rolled, one of the items is chosen in the list, and that value is evaluated at that point.
- `10d8!rr<(2d4)` - every time a die value is potentially rerolled, the value that it is being checked against is evaluated.
- `10d{1d4,1d4+5}!rr<(2d4)` - when evaluation begins, the values for the custom die are calculated. Every time a die value is potentially rerolled, the value that it is being checked against is evaluated.

With the introduction of this notation, it is worth noting that the normal (without exclamation mark) operation of dice means that the values are evaluated once, at the beginning, and then those values are used for the rest of the operation of the program. Additionally, lazily evaluating will often lead to excessive amounts of RNG calls, which can easily exceed the maximum.

## Lists

As well as simple expressions, basic list expressions can be formed. You can form a basic list using `{e,f,g}`, where `e`, `f`, and `g` are expressions as seen before. Additionally, by using `N#YdX` syntax, you can roll `N` amount of dice following `YdX`.

As an addendum to custom dice, if a list value is bracketed then it can be used in custom dice. For example, `5d(4#4d6)` rolls five dice, whose sides are determined by rolling 4d6 4 times. Do note that laziness still applies here, meaning that the RNG cap can be very quickly reached.

## Functions

Here are all the functions, what they take, and what they return.

### Returns an Integer
- abs (integer) - the absolute value of an integer
- fact (integer < 50) - the factorial of an integer
- id (integer) - the integer
- maximum (list) - get the maximum item in a list
- minimum (list) - get the minimum item in a list
- mod (two integers, second /= 0) - get the modulo of two integers
- neg (integer) - the negation of an integer
- sum (list) - the summation of all values in a list
- length (list) - the length of the list
- index (integer (within list bounds), list) - get the item at a given index in the list, 0 indexed

### Returns a List
- drop (integer, list) - drop the first `n` values from a list, where `n` is the integer given
- reverse (list) - reverse the list
- sort (list) - sort the list in ascending order
- take (integer, list) - take the first `n` values from a list, where `n` is the integer given
