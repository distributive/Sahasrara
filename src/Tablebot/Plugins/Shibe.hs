-- |
-- Module      : Tablebot.Plugins.Shibe
-- Description : A very simple plugin that provides shibe pictures.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which just responds with a shibe photo to a .shibe call
module Tablebot.Plugins.Shibe (shibePlugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, eitherDecode)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (httpLBS)
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.SmartCommand (parseComm)
import Tablebot.Plugin.Types
  ( Command,
    DatabaseDiscord,
    EnvCommand (Command),
    EnvPlugin (..),
    HelpPage (HelpPage),
    Plugin,
    RequiredPermission (None),
    plug,
  )

-- | @ShibeAPI@ is the basic data type for the JSON object that the Shibe API returns
type ShibeAPI = Text

-- | @shibe@ is a command that takes no arguments (using 'noArguments') and
-- replies with an image of a shibe. Uses https://shibe.online/ for shibe images.
shibe :: Command
shibe =
  Command
    "shibe"
    (parseComm sendShibe)
    []
  where
    sendShibe :: Message -> DatabaseDiscord ()
    sendShibe m = do
      r <- liftIO (getShibeAPI <&> getShibe)
      sendMessage m r

-- | @getShibeAPI@ is a helper function that turns gets a JSON object that may
-- contain a shibe image. Uses https://shibe.online/ for shibes
getShibeAPI :: IO (Either String ShibeAPI)
getShibeAPI = do
  initReq <- parseRequest "http://shibe.online/api/shibes?count=1&urls=true&httpsUrls=true"
  res <- httpLBS initReq
  return $ ((eitherDecode $ responseBody res) :: Either String [ShibeAPI]) >>= eitherHead
  where
    eitherHead [] = Left "Empty list"
    eitherHead (x : _) = Right x

-- | @getShibe@ is a helper function that turns the Either of @getShibeAPI@
-- into either an error message or the url of the shibe image.
getShibe :: Either String ShibeAPI -> Text
getShibe esc = case esc of
  (Left r) -> "no shibe today, sorry :(. (error is `" <> pack r <> "`)"
  (Right r) -> r

-- | @shibeHelp@ has the help text for the shibe command
shibeHelp :: HelpPage
shibeHelp = HelpPage "shibe" "displays an image of a shibe" "**Shibe**\nGets a random shibe image using <https://shibe.online//>.\n\n*Usage:* `shibe`" [] None

-- | @shibePlugin@ assembles these commands into a plugin containing shibe
shibePlugin :: Plugin
shibePlugin = (plug "shibe") {commands = [shibe], helpPages = [shibeHelp]}
