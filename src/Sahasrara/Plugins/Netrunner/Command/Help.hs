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

import Data.Map (keys)
import Data.Text (Text, intercalate, pack)
import Sahasrara.Plugins.Netrunner.Utility.Search (shorthands)
import Sahasrara.Utility
import Text.RawString.QQ (r)

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
    horoscopeHelp
  ]

shortcutsList :: Text
shortcutsList = intercalate ", " $ map pack $ keys shorthands

searchHelp :: HelpPage
searchHelp =
  HelpPage
    "search"
    []
    "gets a list of all Netrunner cards matching a search query"
    ( [r|Gets a list of all Netrunner cards matching a search query, matching NetrunnerDB's [syntax](<https://netrunnerdb.com/en/syntax>).
Queries are case insensitive and show a maximum of 10 results. There is some shorthand you can use to simplify searches.

The following fields are not implemented:
> `r` - release date
> `z` - rotation

**Usage**
`search x:advanced` all cards containing the text "advanced"
`search o:1 f:nbn"` all 1-cost cards in NBN
`search a:"and the"` all cards with "and the" in their flavour text
`search premium jinteki ice` all non-zero-cost Jinteki ice

**Accepted shorthand queries**
|]
        <> shortcutsList
    )
    []
    None

randomHelp :: HelpPage
randomHelp =
  HelpPage
    "random"
    []
    "randomly selects a card with optional conditions"
    ( [r|Displays a random card from throughout Netrunner's history. NetrunnerDB [syntax](<https://netrunnerdb.com/en/syntax>) may be added to restrict the selection of cards.
Queries are case insensitive, and there is some shorthand you can use to simplify commands.

The following fields are not implemented:
> `r` - release date
> `z` - rotation

**Usage**
`random` displays a random Netrunner card
`random t:agenda` displays a random agenda
`random o:5 f:-` displays a random 5-cost neutral card from either side
`random _:"green level clearance"` displays Green Level Clearance
`random free event` all 0-cost events

**Accepted shorthand queries**
|]
        <> shortcutsList
    )
    []
    None

banListHelp :: HelpPage
banListHelp =
  HelpPage
    "banlist"
    ["bl", "mwl"]
    "lists all cards affected by a given banlist"
    [r|Shows the list of cards affected by the given banlist.
`latest` and `active` will provide their respective banlists (they differ only when the latest banlist has not yet been made active). If no argument is given it will instead list all banlists from Netrunner history.

**Usage**
`banlist` lists all Standard MWL entries
`banlist name` displays the history of the banlist version matching "name"|]
    []
    None

setsHelp :: HelpPage
setsHelp =
  HelpPage
    "sets"
    []
    "lists which sets a card was released in"
    [r|Lists all sets a card was released in, excluding System Core 19.

**Usage**
`sets` lists all Netrunner sets
`sets Hedge Fund` shows the sets *Hedge Fund* was printed in|]
    []
    None

cyclesHelp :: HelpPage
cyclesHelp =
  HelpPage
    "cycles"
    []
    "lists the packs of a given cycle"
    [r|Lists which packs are in a given cycle.

**Usage**
`cycles` lists all cycles
`cycles ashes` shows the packs from the Ashes cycle|]
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
