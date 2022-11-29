-- |
-- Module      : Sahasrara.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Sahasrara.Plugins.Netrunner.Command.Help (helpPageRoots) where

import Sahasrara.Utility
import Text.RawString.QQ (r)
import Data.Text

-- | @helpPageRoots@ encapsulates the help page forest for all Netrunner commands.
helpPageRoots :: [HelpPage]
helpPageRoots =
  [ searchHelp,
    randomHelp,
    setsHelp,
    cyclesHelp,
    banListHelp,
    glossaryHelp,
    ramHelp,
    jankHelp,
    horoscopeHelp
  ]

searchHelp :: HelpPage
searchHelp =
  HelpPage
    "search"
    []
    "gets a list of all Netrunner cards matching a search query"
    searchBody
    []
    None

randomHelp :: HelpPage
randomHelp =
  HelpPage
    "random"
    []
    "randomly selects a card with optional conditions"
    randomBody
    []
    None

searchBody :: Text
searchBody = [r|Gets a list of all Netrunner cards matching a search query, matching NetrunnerDB's upcoming [syntax](api-preview.netrunnerdb.com).
Queries are case insensitive and show a maximum of 10 results.

Note that this uses NetrunnerDB's [new API](api-preview.netrunnerdb.com/api/docs/#printings-filter___printing_search_operator) which is still unstable and subject to changes and bugs.

**Usage**
`search query` gets all cards matching the query

|] <> searchSyntaxDocs

randomBody :: Text
randomBody = [r|Displays a random card. NetrunnerDB [syntax](<https://netrunnerdb.com/en/syntax>) may be added to restrict the selection of cards. Queries are case insensitive.

Note that this uses NetrunnerDB's [new API](api-preview.netrunnerdb.com) which is still unstable and subject to changes and bugs.

**Usage**
`random` query gets a random card matching the query

|] <> searchSyntaxDocs

searchSyntaxDocs :: Text
searchSyntaxDocs = [r|**Syntax**
For full documentation, see the [new API](api-preview.netrunnerdb.com/api/docs/#printings-filter___printing_search_operator). Note that this syntax is a superset of the old NetrunnerDB syntax, and queries valid in the current [search syntax](https://netrunnerdb.com/en/syntax) for NetrunnerDB should be valid for this command. The exception to this is searching by card ID or card aliases.

Search queries are constructed of conditions, taking one of 2 forms:
`card name` (e.g. `sahas` to match Sahasrara) which matches a card titles
`field:value` (e.g. `title:sahas`) which matches specific properties of cards

Field/value pairs have multiple operators:
`field:value` gets cards where the given field has the given value
`field!value` gets cards where the given field doesn't have the given value

It also supports inequalities for numeric and date values:
`field<value`
`field>value`
`field<=value`
`field>=value`

Values can be text, quoted text, numbers, or regular expressions:
`text:abc`
`text:"a b c"`
`text:'a b c'`
`cost:1`
`text:/^[a-z]{10}$/`

You can use multiple conditions to find cards that match all of them:
`a b text:c` gets all cards with `a` _and_ `b` in their title, and also `c` in their text

You can also use `and` and `or`:
`a and b or c and d` gets all cards that either contain both `a` and `b` in their title, or contain both `c` and `d`

Not that using `and` is equivalent to ommitting it entirely, but can be used for clarity:
`a and b or c and d` = `a b or c d`

You can use brackets to specify precedence:
`a and (b or c) and d` gets all cards with `a`, `d`, and either `b` or `c` in their title

You can preface a condition with `-` or `!` to negate it:
`-a` gets all cards without `a` in their title
`-(cost<4 and faction:nbn)` gets all cards that aren't NBN cards costing less than 4

You can add multiple values to the right side of a field/value pair using `&` and `|`:
(They represent `and` and `or` respectively)
`flavour:x&y|z` gets cards with either `x` and `y` in their flavour text, or `z` in their flavour text.

See the API documentation for a full list of valid fields.|]

banListHelp :: HelpPage
banListHelp =
  HelpPage
    "banlist"
    ["bl", "mwl"]
    "lists all cards affected by a given banlist or all banlists of a given format"
    [r|Shows the list of cards affected by a given banlist.
If Startup, Standard, or Eternal is given instead, it will list the banlists of that format.
`latest` and `active` will provide the latest and active Standard banlists (they differ only when the latest banlist has not yet been made active). If no argument is given it will instead list all Standard banlists.

**Usage**
`banlist` lists all Standard banlists and MWLs
`banlist eternal` lists all Eternal Points List entries
`banlist active` displays the active Standard banlist
`banlist name` displays the history of the banlist version matching "name"|]
    []
    None

setsHelp :: HelpPage
setsHelp =
  HelpPage
    "sets"
    []
    "lists which sets a card was released in"
    [r|Lists all sets a card was released in, or all cards released in a specific cycle.

**Usage**
`sets` lists all Netrunner sets
`sets Hedge Fund` shows the sets *Hedge Fund* was printed in
`sets Ashes` lists the sets released in the *Ashes* cycle|]
    []
    None

cyclesHelp :: HelpPage
cyclesHelp =
  HelpPage
    "cycles"
    []
    "lists all cycles"
    [r|Lists all cycles.

**Usage**
`cycles` lists all cycles|]
    []
    None

glossaryHelp :: HelpPage
glossaryHelp =
  HelpPage
    "glossary"
    ["g"]
    "looks up Netrunner definitions"
    [r|Looks up Netrunner terminology. This is a curated list, so please suggest any definitions you'd like to see added or amended.

**Usage**
`glossary <term>` gets the definition of a given term
`glossary` lists contributors to the glossary|]
    []
    None

ramHelp :: HelpPage
ramHelp =
  HelpPage
    "ram"
    []
    "generates RAM card pools"
    [r|RAM (Random Access Memories) is a format where the card pool is randomly generated from a subset of data packs and larger releases of Netrunner cards. See the official [supported formats page](https://nisei.net/players/supported-formats/#random-access-memories) for more.

**Usage**
`ram` generates a standard RAM card pool with 2 large expansions and 12 data packs
`ram x y` generates a customised RAM card pool with `x` large expansions and `y` data packs|]
    []
    None

jankHelp :: HelpPage
jankHelp =
  HelpPage
    "jank"
    []
    "generates a janky combo"
    [r|Generates a janky combo to build around, comprised of an identity and three valid cards for it.

**Usage**
`jank` generates a random ID and three valid cards to go in a deck with it
`jank corp` generates a Corp combo
`jank runner` generates a Runner combo|]
    []
    None

horoscopeHelp :: HelpPage
horoscopeHelp =
  HelpPage
    "horoscope"
    []
    "daily nuggets of inspiration"
    [r|Each day provides a new inspirational* quote, saying, or proverb from the wonderful world of Netrunner.

_*levels of inspiration may vary_|]
    []
    None
