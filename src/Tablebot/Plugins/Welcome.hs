-- |
-- Module      : Tablebot.Plugins.Welcome
-- Description : A plugin for generating welcome messages.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for generating welcome messages.
module Tablebot.Plugins.Welcome (welcomePlugin) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Yaml (decodeFileEither)
import Data.Yaml.Internal (ParseException)
import GHC.Generics (Generic)
import Tablebot.Utility
import Tablebot.Utility.Discord (sendMessage)
import Tablebot.Utility.Random (chooseOne, chooseOneWeighted)
import Tablebot.Utility.SmartParser (PComm (parseComm))
import Text.Printf (printf)
import Text.RawString.QQ (r)

-- | @SS@ denotes the type returned by the command setup.
-- Here it contains the loaded categories from the yaml file so that only needs to be done once.
type SS = [CategoryClass]

-- | @favourite@ is the user-facing command that generates categories.
favourite :: EnvCommand SS
favourite =
  Command
    "favourite"
    ( parseComm $ \m -> do
        cats <- ask
        cat <- liftIO $ generateCategory =<< randomCategoryClass cats
        let formatted = (\(i, c) -> i ++ " is your favourite:\n> " ++ c ++ "?") cat
        sendMessage m $ pack formatted
    )
    []

favouriteHelp :: HelpPage
favouriteHelp =
  HelpPage
    "favourite"
    []
    "generate a category of things you might have a favourite of"
    [r|**Favourite**
Generate a random category of thing to help inspire welcome messages.

*Usage:* `favourite`|]
    []
    None

data CategoryClass = CategoryClass
  { name :: !String,
    weight :: !(Maybe Int),
    template :: !(Maybe String),
    interrogative :: !(Maybe String),
    values :: ![String]
  }
  deriving (Show, Generic)

newtype FileData = FileData {classes :: [CategoryClass]} deriving (Show, Generic)

instance FromJSON CategoryClass

instance FromJSON FileData

yamlFile :: FilePath
yamlFile = "resources/welcome_messages.yaml"

readCategories :: IO [CategoryClass]
readCategories = do
  cats <- decodeFileEither yamlFile :: IO (Either ParseException FileData)
  return $ case cats of
    Left _ -> []
    Right out -> classes out

randomCategoryClass :: [CategoryClass] -> IO CategoryClass
randomCategoryClass cats = do
  chooseOneWeighted getWeight cats
  where
    getWeight c = case weight c of
      Just x -> x
      Nothing -> length $ values c

generateCategory :: CategoryClass -> IO (String, String)
generateCategory catClass = do
  choice <- chooseOne $ values catClass
  return (getInterrogative catClass, printf (getTemplate catClass) choice)
  where
    getTemplate c = fromMaybe "%s" (template c)
    getInterrogative c = fromMaybe "What" (interrogative c)

welcomeStartUp :: StartUp SS
welcomeStartUp = StartUp $ liftIO readCategories

-- | @welcomePlugin@ assembles these commands into a plugin.
welcomePlugin :: EnvPlugin SS
welcomePlugin = (envPlug "welcome" welcomeStartUp) {commands = [favourite], helpPages = [favouriteHelp]}
