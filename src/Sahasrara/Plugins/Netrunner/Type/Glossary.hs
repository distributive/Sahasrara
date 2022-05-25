-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Glossary
-- Description : The type of the Netrunner glossary.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Glossary type.
module Sahasrara.Plugins.Netrunner.Type.Glossary where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Definition = Definition
  { name :: Text,
    aliases :: [Text],
    short :: Text,
    long :: Text,
    isOfficial :: Bool,
    isObsolete :: Bool,
    related :: [Text],
    citations :: Maybe [Text],
    sources :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

data Glossary = Glossary
  { defs :: [Definition],
    source :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Definition

instance FromJSON Glossary

defaultGlossary :: Glossary
defaultGlossary = Glossary {defs = [], source = ""}
