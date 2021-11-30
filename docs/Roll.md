# Roll Command

The roll command has a staggering amount of flexibility, as well as additional features.

Below are listed the current full capabilities of the bot for rolling dice and evaluation expressions. All operations result in integers.

## Basic Operators

- Addition
- Subtraction
- Multiplication
- Division
- Single input function application (currently for `id`,`abs`, `negate`, and `fact` (factorial) up to some number determined by the bot owner, 50 by default)
- Negation using `-`
- Exponentiation
- Parentheses

## Dice Operations

As well as the arithmetic operators above, dice can be rolled, hence the name of the command.

The basic format for this is `dX` where X is some number, meaning a single die of size X. Multiple dice can be rolled using `YdX`, meaning that Y dice are rolled of size X. Parentheses can be used for both Y and X in this case. If Y is greater than a number determined by the bot owner (150 by default), the roll will not be executed. This is the same number that governs the total amount of RNG calls allowed within a command's execution

There are operators that can be performed on dice rolling, listed below. Wherever there is `X`, means an integer. Wherever there is a `#`, means one of `<`,`=`,`>`.

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
- `4d10dw=10` - roll four ten sided dice, dropping any that equal ten

In addition to the above, there is syntax for rolling dice with arbitrary sides - `d{4,7,19,-5}`. This results in a die that is equally likely to result in four, seven, nineteen, or minus five.

As well as all of the above, there is support for stacking dice. This means that if you write `2d4d5d6`, it will be parsed and executed as `((2d4)d5)d6`. Operations can be applied to the dice in this stack.
