# Roll Command

The roll command has a staggering amount of flexibility, as well as additional features.

Below are listed the current full capabilities of the bot for rolling dice and evaluation expressions. All operations (currently) result in integers or a list. A list of functions is available in the [functions section](#Functions).

You can also generate statistics of an expression. See the [Statistics](#Statistics) section for more information.

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

Lists are limited to 50 items long currently (which is configurable).

## Functions

Here are all the functions, what they take, and what they return.

### Returns an Integer
- abs (integer) - the absolute value of an integer
- fact (integer < 50) - the factorial of an integer
- id (integer) - the integer
- max (integer, integer) - get the maximum item between two items
- min (integer, integer) - get the minimum item between two items
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
- between (integer, integer) - generate a list between the two given integers (inclusive)
- concat (list, list) - concatenate two lists together

# Statistics

As well as generating values, statistics based off of expressions can be found. There is a total time limit of 10 seconds for this command, with 5 seconds given to calculations and 5 seconds given to generating the bar chart.

To get these statistics, calling the `roll` command with the `stats` subcommand will generate the requested statistics. The expression given has to return an integer.

The bot will give the mean, the standard deviation, and the top ten most common values of the distribution, as well as graphing the entire distribution.

For example, the result of calling `roll stats 2d20kh1` (roll two twenty sided dice and keep the higher die) can be seen below.

!["The results of asking for stats of 2d20kh1 (roll two twenty sided dice and keep the highest one). The ten most common rolls are 20 to 11. The mean is 13.825. The standard deviation is about 4.7. The bar chart has values on each integer from 1 to 20, with the height of each bar increasing linearly."](./resources/dicestats_2d20kh1.jpg "the result of asking for stats of 2d20kh1")

(above: The results of asking for stats of 2d20kh1 (roll two twenty sided dice and keep the highest one). The ten most common rolls are 20 to 11. The mean is 13.825. The standard deviation is about 4.7. The bar chart has values on each integer from 1 to 20, with the height of each bar increasing linearly.)

Currently, the statistics generation supports all valid expressions.

If invalid states occur (such as with division by zero, negative exponents, or infinite rerolls) the bot will alert the user only if the entire distribution becomes empty. For example, in `1d20rr<(21-d{0,1})`, half of the time infinite rerolls will occur. In this case, these invalid cases are ignored, as they can never be actually rolled, and the only value output is `20`. If the expression given is instead `1/0`, the entire distribution will be empty, as there is no valid output from this expression.

As well as statistics for a given expression, multiple expressions can be shown in the same instance.

For example, the result of calling `roll stats 2d20kh1 4d6dl1` is as follows.

!["The results of asking for stats of 2d20kh1 and 4d6dl1 (roll two twenty sided dice and keep the highest one, and roll four dice with six sides, and drop the lowest value of each). The most common rolls for each expression are 20 to 16, and 13, 12, 14, 11, and 15. The means are about 13.8 and 12.2. The standard deviation are about 4.7 and 2.8. The bar chart has blue values on each integer from 1 to 20, with the height of each bar increasing linearly, and green values that form a weighted bell curve centered on 13."](./resources/dicestats_2d20kh1_4d6dl1.jpg "the result of asking for stats of 2d20kh1 and 4d6dl1")

(above: The results of asking for stats of 2d20kh1 and 4d6dl1 (roll two twenty sided dice and keep the highest one, and roll four dice with six sides, and drop the lowest value of each). The most common rolls for each expression are 20 to 16, and 13, 12, 14, 11, and 15. The means are about 13.8 and 12.2. The standard deviation are about 4.7 and 2.8. The bar chart has blue values on each integer from 1 to 20, with the height of each bar increasing linearly, and green values that form a weighted bell curve centered on 13.)
