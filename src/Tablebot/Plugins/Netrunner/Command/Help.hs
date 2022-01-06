-- |
-- Module      : Tablebot.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Tablebot.Plugins.Netrunner.Command.Help (helpPageRoots) where

import Tablebot.Utility
import Text.RawString.QQ (r)

-- | @helpPageRoots@ encapsulates the help page forest for all Netrunner commands.
helpPageRoots :: [HelpPage]
helpPageRoots = [netrunnerHelp]

netrunnerHelp :: HelpPage
netrunnerHelp =
  HelpPage
    "netrunner"
    ["nr"]
    "finds and displays Netrunner cards"
    [r|**Netrunner**
Find and displays Netrunner cards
Calling without arguments posts some introductory info about the game

Can be used inline by enclosing a card search query inside curly braces (max five queries per message)
Add additional syntax to the start of the query to fetch only the card's image or flavour text

*Usage:*
  - `netrunner`
  - `{{card name}}        ` -> finds the card with title closest matching "card name"
  - `{{card 1}} {{card 2}}` -> searches for cards matching "card 1" and "card 2"
  - `{{!card image}}      ` -> fetches the image of the card matching "card image"
  - `{{|card flavour}}    ` -> fetches the flavour text of the card matching "card flavour"
  - `{{#banned card}}     ` -> fetches the ban history of the card matching "banned card"|]
    [ findHelp,
      findImgHelp,
      findFlavourHelp,
      searchHelp,
      customHelp,
      banHistoryHelp,
      banListHelp,
      rulesHelp
    ]
    None

findHelp :: HelpPage
findHelp =
  HelpPage
    "find"
    []
    "searches the NetrunnerDB database for cards"
    [r|**Find Netrunner Cards**
Searches the NetrunnerDB database for the card closest matching a given query
Can be used inline by enclosing your query inside curly braces (max five queries per message)
Add additional syntax to the start of the query to fetch only the card's image or flavour text

*Usage:*
  - `netrunner find card name` -> finds the card with title closest matching "card name"
  - `{{card name}}           ` -> the inline version of the above command
  - `{{card 1}} {{card 2}}   ` -> searches for cards matching "card 1" and "card 2"
  - `{{!card image}}         ` -> fetches the image of the card matching "card image"
  - `{{|card flavour}}       ` -> fetches the flavour text of the card matching "card flavour"
  - `{{#banned card}}     ` -> fetches the ban history of the card matching "banned card"|]
    []
    None

findImgHelp :: HelpPage
findImgHelp =
  HelpPage
    "image"
    ["img"]
    "searches the NetrunnerDB database for a card's image"
    [r|**Find Netrunner Card Images**
Searches the NetrunnerDB database for the card closest matching a given query and shows an image of it
Can be used inline by enclosing your query inside curly braces with a `!` (max five queries per message)

*Usage:*
  - `netrunner image card name` -> fetches the image of the card matching "card name"
  - `{{!card name}}           ` -> the inline version of the above command|]
    []
    None

findFlavourHelp :: HelpPage
findFlavourHelp =
  HelpPage
    "flavour"
    []
    "searches the NetrunnerDB database for a card's flavour text"
    [r|**Find Netrunner Card Flavour Text**
Searches the NetrunnerDB database for the card closest matching a given query and shows its flavour text
Can be used inline by enclosing your query inside curly braces with a `|` (max five queries per message)

*Usage:*
  - `netrunner flavour card name` -> fetches the flavour text of the card matching "card name"
  - `{{|card name}}             ` -> the inline version of the above command|]
    []
    None

searchHelp :: HelpPage
searchHelp =
  HelpPage
    "search"
    []
    "gets a list of all Netrunner cards matching a search query"
    [r|**Search Netrunner Cards**
Gets a list of all Netrunner cards matching a search query, where the search query uses NetrunnerDB's syntax:
<https://netrunnerdb.com/en/syntax>
If the list is excessively long, it will display a link to an equivalent search on NetrunnerDB
Searches are case insensitive

The following fields are not implemented:
> `r` - release date
> `z` - rotation

*Usage:*
- `netrunner search x:advanced ` -> all cards containing the text "advanced"
- `netrunner search o:1 f:nbn" ` -> all 1-cost cards in NBN
- `netrunner search a:"and the"` -> all cards with "and the" in their flavour text|]
    []
    None

customHelp :: HelpPage
customHelp =
  HelpPage
    "custom"
    []
    "generates custom Netrunner cards"
    [r|**Create Custom Netrunner Cards**
Generates custom Netrunner cards and formats them like existing cards
The order of card parameters does not matter
If you mispell a card parameter (e.g. "typ" instead of "type") it will attempt to correct it

*Usage:*
- `netrunner custom type:agenda                ` -> creates an agenda
- `netrunner custom title:"Name" text:"Lorem." ` -> creates a card with a title and text
- `netrunner custom faction:"nbn"              ` -> creates a card with a faction
- `netrunner custom keywords:"AP - Hardware"   ` -> creates a card with subtypes
- `netrunner custom advancement:5 points:3     ` -> creates a card with an advancement requirement and agenda points
- `netrunner custom cost:3 trash:2             ` -> creates a card with play/rez cost and trash cost
- `netrunner custom strength:4                 ` -> creates a card with strength
- `netrunner custom minSize:40 maxInf:15 link:2` -> creates a card with a minimum deck size, maximum influence, and link
- `netrunner custom flavour:"Raspberry & mint" ` -> creates a card with flavour text
- `netrunner custom unique:true                ` -> creates a unique card|]
    []
    None

banHistoryHelp :: HelpPage
banHistoryHelp =
  HelpPage
    "banHistory"
    ["bh"]
    "lists the banlist history of a card"
    [r|**Netrunner Card Ban History**
Shows the history of a card's legality in each version of Netrunner's MWL

*Usage:*
- `netrunner banHistory card name` -> displays the history of the card matching "card name"
- `{{#card name}}                ` -> the inline version of the above command|]
    []
    None

banListHelp :: HelpPage
banListHelp =
  HelpPage
    "banList"
    ["bl", "mwl"]
    "lists all cards affected by a given banlist"
    [r|**Netrunner Banlists**
Shows the list of cards affected by the given banlist
"latest" and "active" will provide their respective banlists (they differ only when the latest banlist has not yet been made active)
If no argument is given it will instead list all banlists from Netrunner history

*Usage:*
- `netrunner banList name` -> displays the history of the banlist version matching "name"|]
    []
    None

rulesHelp :: HelpPage
rulesHelp =
  HelpPage
    "rules"
    ["cr"]
    "provides official Netrunner rulings"
    [r|**Netrunner Rulings**
Provides the official rulings on certain aspects of the game
Rulings are from the NISEI Comprehensive Rules v1.5

*Usage:*
- `netrunner rules access` -> Shows the steps of accessing a card
- `netrunner rules breach` -> Shows the steps of breaching a server
- `netrunner rules run` -> Shows the timing structure of a run
- `netrunner rules turn` -> Shows the timing structure of a turn
- `netrunner rules corp` -> Shows the timing structure of a Corp's turn
- `netrunner rules runner` -> Shows the timing structure of a Runner's turn|]
    []
    None
