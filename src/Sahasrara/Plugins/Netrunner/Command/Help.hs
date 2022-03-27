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
    -- customHelp,
    banListHelp,
    rulesHelp
  ]

searchHelp :: HelpPage
searchHelp =
  HelpPage
    "search"
    []
    "gets a list of all Netrunner cards matching a search query"
    [r|Gets a list of all Netrunner cards matching a search query, where the search query uses NetrunnerDB's syntax:
<https://netrunnerdb.com/en/syntax>.
If the list is excessively long, it will display a link to an equivalent search on NetrunnerDB. Searches are case insensitive.

The following fields are not implemented:
> `r` - release date
> `z` - rotation

**Usage**
- `search x:advanced ` -> all cards containing the text "advanced"
- `search o:1 f:nbn" ` -> all 1-cost cards in NBN
- `search a:"and the"` -> all cards with "and the" in their flavour text|]
    []
    None

customHelp :: HelpPage
customHelp =
  HelpPage
    "custom"
    []
    "generates custom Netrunner cards"
    [r|Generates custom Netrunner cards and formats them like existing cards.
The order of card parameters does not matter, and if you mispell a card parameter (e.g. "typ" instead of "type") it will attempt to correct it.

**Usage**
- `custom type:agenda                ` -> creates an agenda
- `custom title:"Name" text:"Lorem." ` -> creates a card with a title and text
- `custom faction:"nbn"              ` -> creates a card with a faction
- `custom keywords:"AP - Hardware"   ` -> creates a card with subtypes
- `custom advancement:5 points:3     ` -> creates a card with an advancement requirement and agenda points
- `custom cost:3 trash:2             ` -> creates a card with play/rez cost and trash cost
- `custom strength:4                 ` -> creates a card with strength
- `custom minSize:40 maxInf:15 link:2` -> creates a card with a minimum deck size, maximum influence, and link
- `custom flavour:"Raspberry & mint" ` -> creates a card with flavour text
- `custom unique:true                ` -> creates a unique card|]
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
- `banlist name` -> displays the history of the banlist version matching "name"|]
    []
    None

rulesHelp :: HelpPage
rulesHelp =
  HelpPage
    "rules"
    ["cr"]
    "provides official Netrunner rulings (CR v1.5)"
    [r|Provides the official rulings on certain aspects of the game. Rulings are from the NISEI Comprehensive Rules v1.5.

**Usage**
- `rules access` -> Shows the steps of accessing a card
- `rules breach` -> Shows the steps of breaching a server
- `rules run` -> Shows the timing structure of a run
- `rules turn` -> Shows the timing structure of a turn
- `rules corp` -> Shows the timing structure of a Corp's turn
- `rules runner` -> Shows the timing structure of a Runner's turn|]
    []
    None
