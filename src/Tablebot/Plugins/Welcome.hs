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
import Tablebot.Plugin.Parser (noArguments, untilEnd)

import qualified Data.ByteString.Lazy as B

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, eitherDecode)
import Data.Yaml (decodeFileEither)
import Data.Yaml.Internal (ParseException)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Text.Printf

-- | @favourite@ is the user-facing command that generates categories.
favourite :: Command
favourite = Command "favourite" (noArguments $ \m -> do
    cat <- liftIO $ generateCategory =<< randomCategoryClass
    _ <- sendMessage m $ pack $ fst cat ++ " is your favourite:\n> " ++ snd cat ++ "?"
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

randomCategoryClass :: IO CategoryClass
randomCategoryClass = do
    cats <- categories
    let weightedCategories = concat $ (\c -> replicate (getWeight c) c) <$> cats
    x <- randomElement weightedCategories
    return x
        where
            getWeight c = case weight c of
                Just x -> x
                Nothing -> length $ values c

generateCategory :: CategoryClass -> IO (String, String)
generateCategory = \catClass -> do
    x <- randomElement $ values catClass
    return (getInterrogative catClass, printf (getTemplate catClass) x)
        where
            getTemplate c = case template c of
                Just x -> x
                Nothing -> "%s"
            getInterrogative c = case interrogative c of
                Just x -> x
                Nothing -> "What"

randomElement :: [a] -> IO a
randomElement = \xs -> do
    index <- randomRIO (0, length xs - 1 :: Int)
    return $ head $ drop index xs

-- | @welcomePlugin@ assembles these commands into a plugin.
welcomePlugin :: Plugin
welcomePlugin = plug { commands = [favourite] }
