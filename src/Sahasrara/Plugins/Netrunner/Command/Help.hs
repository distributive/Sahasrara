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

searchHelp :: HelpPage
searchHelp =
  HelpPage
    "search"
    []
    "gets a list of all Netrunner cards matching a search query"
    [r|Gets a list of all Netrunner cards matching a search query, matching NetrunnerDB's [syntax](<https://netrunnerdb.com/en/syntax>).
Queries are case insensitive and show a maximum of 10 results.

Note that this uses NetrunnerDB's [new API](api-preview.netrunnerdb.com) which is still unstable and subject to changes and bugs.

**Usage**
`search x:advanced` all cards containing the text "advanced"
`search o:1 f:nbn"` all 1-cost cards in NBN
`search a:"and the"` all cards with "and the" in their flavour text
|]
    []
    None

randomHelp :: HelpPage
randomHelp =
  HelpPage
    "random"
    []
    "randomly selects a card with optional conditions"
    [r|Displays a random card. NetrunnerDB [syntax](<https://netrunnerdb.com/en/syntax>) may be added to restrict the selection of cards. Queries are case insensitive.

Note that this uses NetrunnerDB's [new API](api-preview.netrunnerdb.com) which is still unstable and subject to changes and bugs.

**Usage**
`random` displays a random Netrunner card
`random t:agenda` displays a random agenda
`random o:5 f:-` displays a random 5-cost neutral card from either side
`random _:"green level clearance"` displays Green Level Clearance
|]
    []
    None

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
