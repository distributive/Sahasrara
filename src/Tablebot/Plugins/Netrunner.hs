-- |
-- Module      : Tablebot.Plugins.Netrunner
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Tablebot.Plugins.Netrunner (netrunnerPlugin) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, pack)
import Discord.Types
import Tablebot.Handler.Command ()
import Tablebot.Plugin
import Tablebot.Plugin.Discord (formatFromEmojiName, sendEmbedMessage, sendMessage)
import Tablebot.Plugin.Exception (BotException (NetrunnerException), throwBot)
import Tablebot.Plugin.Netrunner
import Tablebot.Plugin.Netrunner.Card (Card)
import Tablebot.Plugin.Netrunner.Custom (customCard)
import Tablebot.Plugin.Netrunner.NrApi (NrApi, getNrApi)
import Tablebot.Plugin.Parser (NrQuery (..), netrunnerCustom, netrunnerQuery)
import Tablebot.Plugin.SmartCommand (PComm (parseComm), Quoted (Qu), RestOfInput1 (ROI1), WithError (WErr))
import Text.RawString.QQ (r)

-- | @netrunner@ is the user-facing command that searches for Netrunner cards.
netrunner :: EnvCommand NrApi
netrunner =
  Command
    "netrunner"
    (parseComm nrComm)
    [nrFind, nrFindImg, commandAlias "img" nrFindImg, nrFindFlavour, nrCustom]
  where
    nrComm ::
      WithError
        "Unknown Netrunner functionality"
        () ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    nrComm _ m = sendMessage m =<< beginnerText

-- | @nrFind@ finds the card with title most closely matching its input.
nrFind :: EnvCommand NrApi
nrFind = Command "find" (parseComm findComm) []
  where
    findComm ::
      WithError "No card title given!" (Either (Quoted Text) (RestOfInput1 Text)) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    findComm (WErr (Left (Qu q))) = sendEmbed q
    findComm (WErr (Right (ROI1 q))) = sendEmbed q
    sendEmbed :: Text -> Message -> EnvDatabaseDiscord NrApi ()
    sendEmbed query m = do
      api <- ask
      embedCard (queryCard api query) m

-- | @nrFindImg@ finds the card with title most closely matching its input and
-- posts a picture of it, if there is one.
nrFindImg :: EnvCommand NrApi
nrFindImg = Command "image" (parseComm findComm) []
  where
    findComm ::
      WithError "No card title given!" (Either (Quoted Text) (RestOfInput1 Text)) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    findComm (WErr (Left (Qu q))) = sendEmbed q
    findComm (WErr (Right (ROI1 q))) = sendEmbed q
    sendEmbed :: Text -> Message -> EnvDatabaseDiscord NrApi ()
    sendEmbed query m = do
      api <- ask
      embedCardImg (queryCard api query) m

-- | @nrFindFlavour@ finds the card with title most closely matching its input and
-- posts a picture of it, if there is one.
nrFindFlavour :: EnvCommand NrApi
nrFindFlavour = Command "flavour" (parseComm findComm) []
  where
    findComm ::
      WithError "No card title given!" (Either (Quoted Text) (RestOfInput1 Text)) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    findComm (WErr (Left (Qu q))) = sendEmbed q
    findComm (WErr (Right (ROI1 q))) = sendEmbed q
    sendEmbed :: Text -> Message -> EnvDatabaseDiscord NrApi ()
    sendEmbed query m = do
      api <- ask
      embedCardFlavour (queryCard api query) m

-- | @nrFindInline@ is the inline version of @nrFind@.
nrFindInline :: EnvInlineCommand NrApi
nrFindInline = InlineCommand nrInlineComm
  where
    nrInlineComm :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    nrInlineComm = do
      queries <- netrunnerQuery
      let limitedQs = if length queries > 5 then take 5 queries else queries
      return $ \m -> mapM_ (\q -> sendEmbed q m) limitedQs
    sendEmbed :: NrQuery -> Message -> EnvDatabaseDiscord NrApi ()
    sendEmbed query m = do
      api <- ask
      case query of
        NrQueryCard q -> embedCard (queryCard api $ pack q) m
        NrQueryImg q -> embedCardImg (queryCard api $ pack q) m
        NrQueryFlavour q -> embedCardFlavour (queryCard api $ pack q) m

-- | @nrCustom@ is a command that lets users generate a card embed out of custom
-- data, for the purpose of creating custom cards.
nrCustom :: EnvCommand NrApi
nrCustom = Command "custom" customPars []
  where
    customPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    customPars = do
      pairs <- netrunnerCustom
      return $ customFunc pairs
    customFunc :: [(String, String)] -> Message -> EnvDatabaseDiscord NrApi ()
    customFunc pairs m = do
      api <- ask
      embedCard (customCard api pairs) m

-- | @embedCard@ takes a card and embeds it in a message.
embedCard :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCard card m = do
  api <- ask
  sendEmbedMessage m "" =<< cardToEmbed api card

-- | @embedCardImg@ takes a card and embeds its image in a message, if able.
embedCardImg :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCardImg card m = do
  api <- ask
  case cardToImgEmbed api card of
    Nothing -> throwBot $ NetrunnerException "Could not get card art"
    Just embed -> sendEmbedMessage m "" embed

-- | @embedCardFlavour@ takes a card and embeds its image in a message, if able.
embedCardFlavour :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCardFlavour card m = do
  api <- ask
  case cardToFlavourEmbed api card of
    Nothing -> throwBot $ NetrunnerException "Card has no flavour text"
    Just embed -> sendEmbedMessage m "" embed

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
  - `{{|card flavour}}    ` -> fetches the flavour text of the card matching "card flavour" |]
    [findHelp, findImgHelp, findFlavourHelp, customHelp]
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
  - `{{|card flavour}}       ` -> fetches the flavour text of the card matching "card flavour" |]
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
  - `{{!card name}}          ` -> the inline version of the above command|]
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
  - `{{|card name}}          ` -> the inline version of the above command|]
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

beginnerText :: EnvDatabaseDiscord NrApi Text
beginnerText = do
  subroutine <- formatFromEmojiName "subroutine"
  agenda <- formatFromEmojiName "agenda"
  rezCost <- formatFromEmojiName "rez_cost"
  return $
    agenda <> " **NETRUNNER** " <> rezCost
      <> [r|
Netrunner is an asymmetric collectable card game about hackers hacking corporations. It's run as a *free* community endeavour by NISEI:
|]
      <> subroutine
      <> [r| <https://nisei.net/>

**Learn to play**
|]
      <> subroutine
      <> [r| <https://nisei.net/players/learn-to-play/>

**Get involved here**
There is a sizeable Netrunner community here of new and old society members. If you want to get into the game feel free to ask in #netrunner for some advice or a beginner game and someone will be happy to help you!|]

netrunnerStartUp :: StartUp NrApi
netrunnerStartUp = StartUp $ liftIO getNrApi

-- | @welcomePlugin@ assembles these commands into a plugin.
netrunnerPlugin :: EnvPlugin NrApi
netrunnerPlugin =
  (envPlug "netrunner" netrunnerStartUp)
    { commands = [netrunner, commandAlias "nr" netrunner],
      inlineCommands = [nrFindInline],
      helpPages = [netrunnerHelp]
    }
