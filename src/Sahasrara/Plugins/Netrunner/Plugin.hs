-- |
-- Module      : Sahasrara.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Sahasrara.Plugins.Netrunner.Plugin (netrunnerPlugin) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Command.BanList
import Sahasrara.Plugins.Netrunner.Command.Find
import Sahasrara.Plugins.Netrunner.Command.Help (helpPageRoots)
import Sahasrara.Plugins.Netrunner.Command.Rules
import Sahasrara.Plugins.Netrunner.Command.Search
import Sahasrara.Plugins.Netrunner.Type.BanList (BanList (active), CardBan (..))
import qualified Sahasrara.Plugins.Netrunner.Type.BanList as BanList
import Sahasrara.Plugins.Netrunner.Type.Card (Card (code, text))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.BanList (activeBanList, latestBanListActive, toMwlStatus)
import Sahasrara.Plugins.Netrunner.Utility.Embed
import Sahasrara.Plugins.Netrunner.Utility.Misc (formatNr)
import Sahasrara.Plugins.Netrunner.Utility.NrApi (getNrApi)
import Sahasrara.Plugins.Netrunner.Utility.Search
import Sahasrara.Utility
import Sahasrara.Utility.Discord (sendEmbedMessage, sendMessage)
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Parser (inlineCommandHelper)
import Sahasrara.Utility.Random (chooseOne)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))
import Sahasrara.Utility.Types ()
import Text.Megaparsec (anySingleBut, some)

-- | @nrInline@ searches for cards by name.
nrInline :: EnvInlineCommand NrApi
nrInline = inlineCommandHelper "[[" "]]" (some $ anySingleBut ']') $ \query m -> do
  api <- ask
  embedCard (queryCard api $ pack query) m

-- | @nrInlineImg@ searches for a card and outputs an image of it.
nrInlineImg :: EnvInlineCommand NrApi
nrInlineImg = inlineCommandHelper "{{" "}}" (some $ anySingleBut '}') $ \query m -> do
  api <- ask
  embedCardImg (queryCard api $ pack query) m

-- | @nrInlineFlavour@ searches for a card and outputs its flavour.
nrInlineFlavour :: EnvInlineCommand NrApi
nrInlineFlavour = inlineCommandHelper "<<" ">>" (some $ anySingleBut '>') $ \query m -> do
  api <- ask
  embedCardFlavour (queryCard api $ pack query) m

-- | @nrInlineBanHistory@ searches for a card and outputs its legality history.
nrInlineBanHistory :: EnvInlineCommand NrApi
nrInlineBanHistory = inlineCommandHelper "((" "))" (some $ anySingleBut ')') $ \query m -> do
  api <- ask
  embedBanHistory (queryCard api $ pack query) m

-- | @nrSearch@ searches the card database with specific queries.
nrSearch :: EnvCommand NrApi
nrSearch = Command "search" searchPars []
  where
    searchPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    searchPars = queryParser $ \cs pairs m -> do
      case cs of
        Nothing -> sendMessage m "No criteria provided!"
        Just [] -> sendMessage m $ "No cards found for `" <> pairsToNrdb pairs <> "`"
        Just [card] -> embedCard card m
        Just cards ->
          embedCards
            ("Query: `" <> pairsToNrdb pairs <> "`\n")
            cards
            ("_[...view on NRDB](" <> pairsToQuery pairs <> ")_")
            ("_[..." <> pack (show $ length cards - 10) <> " more](" <> pairsToQuery pairs <> ")_")
            m

-- | @nrRandom@ searches the card database with specific queries and outputs a
-- single result at random.
nrRandom :: EnvCommand NrApi
nrRandom = Command "random" randomPars []
  where
    randomPars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    randomPars = queryParser $ \cs pairs m -> do
      case cs of
        Nothing -> do
          api <- ask
          card <- liftIO $ chooseOne $ cards api
          embedCard card m
        Just [] -> sendMessage m $ "No cards found for `" <> pairsToNrdb pairs <> "`"
        Just cards -> do
          card <- liftIO $ chooseOne cards
          embedCard card m

-- | @nrBanList@ is a command listing all cards affected by a banlist.
nrBanList :: EnvCommand NrApi
nrBanList = Command "banlist" (parseComm banListComm) []
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
embedCards :: Text -> [Card] -> Text -> Text -> Message -> EnvDatabaseDiscord NrApi ()
embedCards pre cards post err m = do
  api <- ask
  sendEmbedMessage m "" =<< cardsToEmbed api pre cards post err

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
  cText <- formatNr $ fromMaybe "" $ text card'
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

-- | @netrunnerStartUp@ loads the NetrunnerDB api once at start up
netrunnerStartUp :: StartUp NrApi
netrunnerStartUp = StartUp $ liftIO getNrApi

-- | @welcomePlugin@ assembles these commands into a plugin.
netrunnerPlugin :: EnvPlugin NrApi
netrunnerPlugin =
  (envPlug "netrunner" netrunnerStartUp)
    { commands =
        [ nrSearch,
          nrRandom,
          nrBanList,
          commandAlias "bl" nrBanList,
          commandAlias "mwl" nrBanList,
          nrRules,
          commandAlias "cr" nrRules
        ],
      inlineCommands = [nrInline, nrInlineImg, nrInlineFlavour, nrInlineBanHistory],
      helpPages = helpPageRoots
    }
