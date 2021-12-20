-- |
-- Module      : Tablebot.Plugins.Cats
-- Description : A very simple plugin that provides cat pictures.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which just responds with a cat photo to a .cat call
module Tablebot.Plugins.Cats (catPlugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, Object, eitherDecode)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (Response (responseBody), parseRequest)
import Network.HTTP.Simple (addRequestHeader, httpLBS)
import System.Environment (lookupEnv)
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

-- | @CatAPI@ is the basic data type for the JSON object that thecatapi returns
data CatAPI = CatAPI
  { breeds :: ![Object],
    id :: !Text,
    url :: !Text,
    width :: !Int,
    height :: !Int
  }
  deriving (Show, Generic)

instance FromJSON CatAPI

-- | @cat@ is a command that takes no arguments (using 'noArguments') and
-- replies with an image of a cat. Uses https://docs.thecatapi.com/ for cats.
cat :: Command
cat =
  Command
    "cat"
    (parseComm sendCat)
    []
  where
    sendCat :: Message -> DatabaseDiscord ()
    sendCat m = do
      r <- liftIO (getCatAPI <&> getCat)
      sendMessage m r

-- | @getCatAPI@ is a helper function that turns gets a JSON object that may
-- contain a cat image. Uses https://docs.thecatapi.com/ for cats.
-- CATAPI_TOKEN must be provided in .env so that there are no cat limits.
getCatAPI :: IO (Either String CatAPI)
getCatAPI = do
  initReq <- parseRequest "https://api.thecatapi.com/v1/images/search"
  maybeAPIToken <- lookupEnv "CATAPI_TOKEN"
  dToken <- genErr maybeAPIToken
  let req = addRequestHeader "x-api-key" ((encodeUtf8 . pack) dToken) initReq
  res <- httpLBS req
  return $ ((eitherDecode $ responseBody res) :: Either String [CatAPI]) >>= eitherHead
  where
    genErr (Just tok) = return tok
    genErr _ = putStrLn "Could not find CATAPI_TOKEN, using no token" >> return ""
    eitherHead [] = Left "Empty list"
    eitherHead (x : _) = Right x

-- | @getCat@ is a helper function that turns the Either of @getCatAPI@
-- into either an error message or the url of the cat image.
getCat :: Either String CatAPI -> Text
getCat esc = case esc of
  (Left r) -> "no cat today, sorry :(. (error is `" <> pack r <> "`)"
  (Right r) -> url r

-- | @catHelp@ has the help text for the cat command
catHelp :: HelpPage
catHelp = HelpPage "cat" [] "displays an image of a cat" "**Cat**\nGets a random cat image using <https://thecatapi.com/>.\n\n*Usage:* `cat`" [] None

-- | @catPlugin@ assembles these commands into a plugin containing cat
catPlugin :: Plugin
catPlugin = (plug "cats") {commands = [cat], helpPages = [catHelp]}
