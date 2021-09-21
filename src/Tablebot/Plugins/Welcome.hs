{-|
Module      : Tablebot.Plugins.Welcome
Description : Basic commands that just output string literals.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

Commands for generating welcome messages.
-}
module Tablebot.Plugins.Welcome (welcomePlugin) where

import Tablebot.Plugin
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Parser (noArguments)
import Tablebot.Util.Error
import Tablebot.Util.Random (chooseOne, chooseOneWeighted)

import qualified Data.ByteString.Lazy as B

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, eitherDecode)
import Data.Yaml (decodeFileEither)
import Data.Yaml.Internal (ParseException)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Text.Printf

-- | @favourite@ is the user-facing command that generates categories.
favourite :: Command
favourite = Command "favourite" (noArguments $ \m -> do
    cat <- liftIO $ generateCategory =<< randomCategoryClass
    let formatted = either id id $ (\(i, c) -> i ++ " is your favourite:\n> " ++ c ++ "?") <$> cat
    _ <- sendMessage m $ pack $ formatted
    return ())

data CategoryClass = CategoryClass { name :: !String
                                   , weight :: !(Maybe Int)
                                   , template :: !(Maybe String)
                                   , interrogative :: !(Maybe String)
                                   , values :: ![String]
                                   } deriving (Show, Generic)

data FileData = FileData { classes :: ![CategoryClass] } deriving (Show, Generic)

instance FromJSON CategoryClass
instance FromJSON FileData

yamlFile :: FilePath
yamlFile = "resources/welcome_messages.yaml"

categories :: IO [CategoryClass]
categories = do
    cats <- decodeFileEither yamlFile :: IO (Either ParseException FileData)
    return $ case cats of
        Left err -> []
        Right out -> classes out

randomCategoryClass :: IO (Either Error CategoryClass)
randomCategoryClass = do
    cats <- categories
    chooseOneWeighted getWeight cats
        where
            getWeight c = case weight c of
                Just x -> x
                Nothing -> length $ values c

generateCategory :: Either Error CategoryClass -> IO (Either Error (String, String))
generateCategory (Left err) = return $ Left err
generateCategory (Right catClass) = do
    choice <- chooseOne $ values catClass
    return $ (\x -> (getInterrogative catClass, printf (getTemplate catClass) x)) <$> choice
        where
            getTemplate c = case template c of
                Just x -> x
                Nothing -> "%s"
            getInterrogative c = case interrogative c of
                Just x -> x
                Nothing -> "What"

-- | @welcomePlugin@ assembles these commands into a plugin.
welcomePlugin :: Plugin
welcomePlugin = plug { commands = [favourite] }
