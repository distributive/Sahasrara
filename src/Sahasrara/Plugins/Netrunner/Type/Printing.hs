-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Printing type.
module Sahasrara.Plugins.Netrunner.Type.Printing where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn)
import GHC.Generics (Generic)

-- | @Printing@ represents a specific printing of a card in the NetrunnerDB API.
data Printing = Printing
  { code :: !Text,
    cardCode :: !Text,
    cardSetCode :: !Text,
    printedText :: !Text,
    strippedPrintedText :: !Text,
    printedIsUnique :: !Bool,
    flavour :: !(Maybe Text),
    illustrators :: ![Text],
    position :: !Int,
    quantity :: !Int,
    dateRelease :: !Text,
    image :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Printing where
  parseJSON = withObject "Printing" $ \o -> do
    code <- o .: "id"
    a <- o .: "attributes"
    cardCode <- a .: "card_id"
    cardSetCode <- a .: "card_set_id"
    printedText <- do
      t <- a .: "printed_text"
      return $ fromMaybe "" t
    strippedPrintedText <- do
      t <- a .: "stripped_printed_text"
      return $ fromMaybe "" t
    printedIsUnique <- a .: "printed_is_unique"
    flavour <- a .:? "flavor"
    illustrators <- do
      i <- a .:? "display_illustrators"
      return $ case i of
        Nothing -> []
        Just i' -> splitOn ", " i'
    position <- a .: "position"
    quantity <- a .: "quantity"
    dateRelease <- a .: "date_release"
    image <- do
      images <- a .: "images"
      nrdb <- images .: "nrdb_classic"
      url <- nrdb .: "large"
      return url
    return Printing {..}
