-- |
-- Module      : Sahasrara.Plugins.Netrunner.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of all Netrunner data in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.NrApi (getNrApi) where

import Data.Aeson (FromJSON)
import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Yaml (decodeFileEither)
import Data.Yaml.Internal (ParseException)
import GHC.Generics (Generic)
import Sahasrara.Plugins.Netrunner.Type.Blacklist (Blacklist, defaultBlacklist)
import Sahasrara.Plugins.Netrunner.Type.Glossary (Glossary, defaultGlossary)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Utility.Json (Content (content), contentRequest)

-- | @getNrApi@ is a function that attempts to get the JSON objects containing
-- all required Netrunner data (cards, cycles, and packs) as provided by
-- https://netrunnerdb.com/api/2.0/doc.
getNrApi :: IO NrApi
getNrApi = do
  cards <- content <$> contentRequest "Cards" "cards" []
  printings <- content <$> contentRequest "Printings" "printings" ["sort=date_release"] >>= return . reverse -- Applying the reverse to the API directly with "sort=-date_release" causes some inconsistent bugs I don't understand
  cardTypes <- content <$> contentRequest "CardTypes" "card_types" []
  factions <- content <$> contentRequest "Factions" "factions" []
  cardCycles <- content <$> contentRequest "CardCycles" "card_cycles" ["sort=date_release"]
  cardSets <- content <$> contentRequest "CardSets" "card_sets" []
  formats <- content <$> contentRequest "Formats" "formats" []
  snapshots <- content <$> contentRequest "Snapshots" "snapshots" []
  cardPools <- content <$> contentRequest "CardPools" "card_pools" []
  restrictions <- content <$> contentRequest "Restrictions" "restrictions" []
  cardAliases <- getAliases
  putStrLn "Loaded aliases"
  blacklist <- getBlacklist
  putStrLn "Loaded blacklist"
  glossary <- getGlossary
  putStrLn "Loaded glossary"
  return NrApi {..}

-- | @AliasFile@ represents the raw alias data.
data AliasFile = AliasFile {aliases :: Map Text Text} deriving (Show, Generic)

instance FromJSON AliasFile

-- | @getAliases@ loads the alias file.
getAliases :: IO (Map Text Text)
getAliases = do
  as <- decodeFileEither yamlFile :: IO (Either ParseException AliasFile)
  return $ case as of
    Left _ -> empty
    Right out -> aliases out
  where
    yamlFile :: FilePath
    yamlFile = "resources/aliases.yaml"

-- | @getBlacklist@ loads the blacklist file.
getBlacklist :: IO Blacklist
getBlacklist = do
  bl <- decodeFileEither yamlFile :: IO (Either ParseException Blacklist)
  return $ case bl of
    Left _ -> defaultBlacklist
    Right out -> out
  where
    yamlFile :: FilePath
    yamlFile = "resources/horoscopeBlacklist.yaml"

-- | @getGlossary@ loads the glossary file.
getGlossary :: IO Glossary
getGlossary = do
  glossary <- decodeFileEither yamlFile :: IO (Either ParseException Glossary)
  return $ case glossary of
    Left _ -> defaultGlossary
    Right out -> out
  where
    yamlFile :: FilePath
    yamlFile = "resources/glossary.yaml"
