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
import Data.Aeson (eitherDecode)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
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
    commandAlias,
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

-- | @birb@ is a command that takes no arguments (using 'noArguments') and
-- replies with an image of a birb. Uses https://shibe.online/ for birb images.
birb :: Command
birb =
  Command
    "birb"
    (parseComm sendBirb)
    []
  where
    sendBirb :: Message -> DatabaseDiscord ()
    sendBirb m = do
      r <- liftIO (getBirbAPI <&> getBirb)
      sendMessage m r

-- | @getShibeAPI@ is a helper function that turns gets a JSON object that may
-- contain a shibe image. Uses https://shibe.online/ for shibes
getShibeAPI :: IO (Either String ShibeAPI)
getShibeAPI = getAPI "http://shibe.online/api/shibes?count=1&urls=true&httpsUrls=true"

-- | @getShibeAPI@ is a helper function that turns gets a JSON object that may
-- contain a shibe image. Uses https://shibe.online/ for shibes
getBirbAPI :: IO (Either String ShibeAPI)
getBirbAPI = getAPI "http://shibe.online/api/birds?count=1&urls=true&httpsUrls=true"

-- | @getAPI@ is a helper function that turns gets a JSON object that may
-- contain an image offered by https://shibe.online.
getAPI :: String -> IO (Either String ShibeAPI)
getAPI url = do
  initReq <- parseRequest url
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

-- | @getBirb@ is a helper function that turns the Either of @getShibeAPI@
-- into either an error message or the url of the bird image.
getBirb :: Either String ShibeAPI -> Text
getBirb esc = case esc of
  (Left r) -> "no shibe today, sorry :(. (error is `" <> pack r <> "`)"
  (Right r) -> r

-- | @shibeHelp@ has the help text for the shibe command
shibeHelp :: HelpPage
shibeHelp = HelpPage "shibe" [] "displays an image of a shibe" "**Shibe**\nGets a random shibe image using <https://shibe.online/>.\n\n*Usage:* `shibe`" [] None

-- | @shibeHelp@ has the help text for the shibe command
birbHelp :: HelpPage
birbHelp = HelpPage "bird" [] "displays an image of a bird" "**Bird**\nGets a random bird image using <https://shibe.online/>.\n\n*Usage:* `bird`" [] None

-- | @shibePlugin@ assembles these commands into a plugin containing shibe
shibePlugin :: Plugin
shibePlugin = (plug "shibe") {commands = [birb, commandAlias "bird" birb, shibe], helpPages = [birbHelp, shibeHelp]}
