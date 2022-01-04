-- |
-- Module      : Tablebot.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Tablebot.Plugins.Netrunner.Plugin (netrunnerPlugin) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, pack, uncons)
import Discord.Types
import Tablebot.Internal.Handler.Command ()
import Tablebot.Plugins.Netrunner.Command.BanList
import Tablebot.Plugins.Netrunner.Command.Custom
import Tablebot.Plugins.Netrunner.Command.Find
import Tablebot.Plugins.Netrunner.Command.Help (helpPageRoots)
import Tablebot.Plugins.Netrunner.Command.Rules
import Tablebot.Plugins.Netrunner.Command.Search
import Tablebot.Plugins.Netrunner.Type.BanList (BanList (active), CardBan (..))
import qualified Tablebot.Plugins.Netrunner.Type.BanList as BanList
import Tablebot.Plugins.Netrunner.Type.Card (Card (code))
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Tablebot.Plugins.Netrunner.Utility.BanList (activeBanList, latestBanListActive, toMwlStatus)
import Tablebot.Plugins.Netrunner.Utility.Card (toText)
import Tablebot.Plugins.Netrunner.Utility.Embed
import Tablebot.Plugins.Netrunner.Utility.NrApi (getNrApi)
import Tablebot.Utility
import Tablebot.Utility.Discord (formatFromEmojiName, sendEmbedMessage, sendMessage)
import Tablebot.Utility.Embed (addColour)
import Tablebot.Utility.Parser (inlineCommandHelper, keyValue, keyValuesSepOn)
import Tablebot.Utility.SmartParser (PComm (parseComm), Quoted (Qu), RestOfInput (ROI), RestOfInput1 (ROI1), WithError (WErr))
import Tablebot.Utility.Types ()
import Text.Megaparsec (anySingleBut, some)
import Text.RawString.QQ (r)

-- | @netrunner@ is the user-facing command that searches for Netrunner cards.
netrunner :: EnvCommand NrApi
netrunner =
  Command
    "netrunner"
    (parseComm nrComm)
    [ nrFind,
      nrFindImg,
      commandAlias "img" nrFindImg,
      nrFindFlavour,
      nrSearch,
      nrCustom,
      nrBanHistory,
      commandAlias "bh" nrBanHistory,
      nrBanList,
      commandAlias "bl" nrBanList,
      commandAlias "mwl" nrBanList,
      nrRules,
      commandAlias "cr" nrRules
    ]
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

-- | @nrInline@ is the inline version of the commands that find cards.
nrInline :: EnvInlineCommand NrApi
nrInline = inlineCommandHelper "{{" "}}" (some $ anySingleBut '}') $ \query m -> do
  api <- ask
  case uncons $ pack query of
    Just ('!', q) -> embedCardImg (queryCard api q) m
    Just ('|', q) -> embedCardFlavour (queryCard api q) m
    Just ('#', q) -> embedBanHistory (queryCard api q) m
    _ -> embedCard (queryCard api $ pack query) m

-- | @nrSearch@ searches the card database with specific queries.
nrSearch :: EnvCommand NrApi
nrSearch = Command "search" searchPars []
  where
    searchPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    searchPars = do
      ps <- keyValuesSepOn [':', '<', '>', '!'] ['|']
      return $ \m -> do
        api <- ask
        let pairs = fixSearch api ps
        case searchCards api pairs of
          Nothing -> sendMessage m "No criteria provided!"
          Just [] -> sendMessage m $ "No cards found for `" <> pairsToNrdb pairs <> "`"
          Just [res] -> embedCard res m
          Just res ->
            embedCards
              ("Query: `" <> pairsToNrdb pairs <> "`\n")
              res
              ("_[..." <> pack (show $ length res - 10) <> " more](" <> pairsToQuery pairs <> ")_")
              m

-- | @nrCustom@ is a command that lets users generate a card embed out of custom
-- data, for the purpose of creating custom cards.
nrCustom :: EnvCommand NrApi
nrCustom = Command "custom" customPars []
  where
    customPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    customPars = do
      pairs <- keyValue
      return $ \m -> do
        api <- ask
        embedCard (customCard api pairs) m

-- | @nrBanHistory@ is a command that lists a card's banlist history.
nrBanHistory :: EnvCommand NrApi
nrBanHistory = Command "banHistory" (parseComm banHistoryComm) []
  where
    banHistoryComm ::
      WithError "No card title given!" (RestOfInput1 Text) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    banHistoryComm (WErr (ROI1 q)) = sendEmbed q
    sendEmbed :: Text -> Message -> EnvDatabaseDiscord NrApi ()
    sendEmbed query m = do
      api <- ask
      embedBanHistory (queryCard api query) m

-- | @nrBanList@ is a command listing all cards affected by a banlist.
nrBanList :: EnvCommand NrApi
nrBanList = Command "banList" (parseComm banListComm) []
  where
    banListComm ::
      Either () (RestOfInput Text) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    banListComm (Left ()) = embedBanLists
    banListComm (Right (ROI q)) = sendEmbed q
    sendEmbed :: Text -> Message -> EnvDatabaseDiscord NrApi ()
    sendEmbed query m = do
      api <- ask
      embedBanList (queryBanList api query) m

-- | @nrRules@ is a command that fetches Netrunner rulings.
nrRules :: EnvCommand NrApi
nrRules = Command "rules" (parseComm rulesComm) []
  where
    rulesComm :: RestOfInput Text -> Message -> EnvDatabaseDiscord NrApi ()
    rulesComm (ROI q) m = do
      let (rTitle, rBody, colour) = case getRuling q of
            Left (Ruling t b) -> (t, b, Red)
            Right (Ruling t b) -> (t, b, Blue)
      sendEmbedMessage m "" $ addColour colour $ embedText rTitle rBody

-- | @embedCard@ takes a card and embeds it in a message.
embedCard :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCard card m = do
  api <- ask
  sendEmbedMessage m "" =<< cardToEmbed api card

-- | @embedCards@ takes a list of cards and embeds their names.
embedCards :: Text -> [Card] -> Text -> Message -> EnvDatabaseDiscord NrApi ()
embedCards pre cards err m = do
  api <- ask
  sendEmbedMessage m "" =<< cardsToEmbed api pre cards err

-- | @embedCardImg@ embeds a card's image in a message, if able.
embedCardImg :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCardImg card m = do
  api <- ask
  sendEmbedMessage m "" $ cardToImgEmbed api card

-- | @embedCardFlavour@ embeds a card's flavour in a message, if able.
embedCardFlavour :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedCardFlavour card m = do
  api <- ask
  let card' = case code card of
        Just "07024" -> queryCard api "Déjà Vu"
        Just "01002" -> queryCard api "The Twins"
        _ -> card
  cText <- toText card'
  embed <- case code card' of
    Just "12077" -> cardToEmbedWithText api card' cText
    _ -> cardToFlavourEmbed api card'
  sendEmbedMessage m "" embed

-- | @embedBanHistory@ embeds a card's banlist history.
embedBanHistory :: Card -> Message -> EnvDatabaseDiscord NrApi ()
embedBanHistory card m = do
  api <- ask
  embed <- cardToEmbedWithText api card $ listBanHistory api card
  let colour = case toMwlStatus api (activeBanList api) card of
        Banned -> Red
        Legal -> Green
        _ -> Yellow
  sendEmbedMessage m "" $ addColour colour embed

-- | @embedBanLists@ embeds all banlists in Netrunner history.
embedBanLists :: Message -> EnvDatabaseDiscord NrApi ()
embedBanLists m = do
  api <- ask
  let embed = embedTextWithUrl "Standard Banlists" "https://netrunnerdb.com/en/banlists" $ listBanLists api
      colour = if latestBanListActive api then Red else Yellow
  sendEmbedMessage m "" $ addColour colour embed

-- | @embedBanList@ embeds the list of cards affected by a given banlist.
embedBanList :: BanList -> Message -> EnvDatabaseDiscord NrApi ()
embedBanList banList m = do
  api <- ask
  let (pre, cCards, rCards) = listAffectedCards api banList
      header = BanList.name banList <> if active banList then " (active)" else ""
      colour = if active banList then Red else Yellow
  sendEmbedMessage m "" $ addColour colour $ embedColumns header pre [("Corp Cards", cCards), ("Runner Cards", rCards)]

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
      inlineCommands = [nrInline],
      helpPages = helpPageRoots
    }
