{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Tablebot.Plugins.Dog
-- Description : A very simple plugin that provides dog pictures.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which just responds with a dog photo to a .dog call
module Tablebot.Plugins.Dogs (dogPlugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
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

-- | @dog@ is a command that takes no arguments (using 'noArguments') and
-- replies with an image of a dog. Uses https://randomdog.ca/ for dog images.
dog :: Command
dog =
  Command
    "dog"
    (parseComm sendDog)
    []
  where
    sendDog :: Message -> DatabaseDiscord ()
    sendDog m = do
      r <- liftIO getDogAPI
      sendMessage m r

-- | @getAPI@ is a helper function that turns gets a JSON object that may
-- contain an image offered by https://shibe.online.
getDogAPI :: IO Text
getDogAPI = do
  initReq <- parseRequest "https://random.dog/woof/"
  res <- httpLBS initReq
  return $ "https://random.dog/" <> toStrict (decodeUtf8 (responseBody res))

-- | @dogHelp@ has the help text for the dog command
dogHelp :: HelpPage
dogHelp = HelpPage "dog" [] "displays an image of a dog" "**Dog**\nGets a random dog image using <https://random.dog/>.\n\n*Usage:* `dog`" [] None

-- | @dogPlugin@ assembles these commands into a plugin containing dog
dogPlugin :: Plugin
dogPlugin = (plug "dog") {commands = [dog], helpPages = [dogHelp]}
