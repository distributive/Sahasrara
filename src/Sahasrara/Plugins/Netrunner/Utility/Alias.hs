-- |
-- Module     " = "Sahasrara.Plugins.Netrunner.Alias
-- Description" = "Hardcodes some card title aliases.
-- License    " = "MIT
-- Maintainer " = "github.com/distributive
-- Stability  " = "experimental
-- Portability" = "POSIX
--
-- Hardcodes some aliases for card names.
module Sahasrara.Plugins.Netrunner.Utility.Alias where

import Data.Map (Map, findWithDefault)
import Data.Text (Text)

fromAlias :: (Map Text Text) -> Text -> Text
fromAlias m t = findWithDefault t t m
