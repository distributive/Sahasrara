{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Tablebot.Plugins.Fox
-- Description : A very simple plugin that provides fox pictures.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which just responds with a fox photo to a .fox call
module Tablebot.Plugins.Fox (foxPlugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, eitherDecode)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)
import Tablebot.Utility.Discord (Message, sendMessage)
import Tablebot.Utility.SmartParser (parseComm)
import Tablebot.Utility.Types
  ( Command,
    DatabaseDiscord,
    EnvCommand (Command),
    EnvPlugin (..),
    HelpPage (HelpPage),
    Plugin,
    RequiredPermission (None),
    plug,
  )

-- | @FoxAPI@ is the basic data type for the JSON object that the Fox API returns
data FoxAPI = Fox
  { image :: Text,
    link :: Text
  }
  deriving (Generic, Show)

instance FromJSON FoxAPI

-- | @fox@ is a command that takes no arguments (using 'noArguments') and
-- replies with an image of a fox. Uses https://randomfox.ca/ for fox images.
fox :: Command
fox =
  Command
    "fox"
    (parseComm sendFox)
    []
  where
    sendFox :: Message -> DatabaseDiscord ()
    sendFox m = do
      r <- liftIO (getFoxAPI <&> getFox)
      sendMessage m r

-- | @getAPI@ is a helper function that turns gets a JSON object that may
-- contain an image offered by https://shibe.online.
getFoxAPI :: IO (Either String FoxAPI)
getFoxAPI = do
  initReq <- parseRequest "https://randomfox.ca/floof/"
  res <- httpLBS initReq
  return ((eitherDecode $ responseBody res) :: Either String FoxAPI)

-- | @getFox@ is a helper function that turns the Either of @getFoxAPI@
-- into either an error message or the url of the fox image.
getFox :: Either String FoxAPI -> Text
getFox esc = case esc of
  (Left r) -> "no fox today, sorry :(. (error is `" <> pack r <> "`)"
  (Right r) -> image r

-- | @foxHelp@ has the help text for the fox command
foxHelp :: HelpPage
foxHelp = HelpPage "fox" [] "displays an image of a fox" "**Fox**\nGets a random fox image using <https://randomfox.ca/>.\n\n*Usage:* `fox`" [] None

-- | @foxPlugin@ assembles these commands into a plugin containing fox
foxPlugin :: Plugin
foxPlugin = (plug "fox") {commands = [fox], helpPages = [foxHelp]}
