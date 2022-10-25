-- |
-- Module      : Sahasrara.Plugins.Netrunner.Card
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Handles the representation of Netrunner cards in Sahasrara.
module Sahasrara.Plugins.Netrunner.Utility.Printing
  ( toCard,
    toLink,
    toCardSet,
    toCycle,
    toFlavour,
    toImage,
    formatPrinting,
    toReleaseData,
  )
where

import Control.Monad.Trans.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Discord.Types
import Safe
import Sahasrara.Plugins.Netrunner.Type.Card (Card)
import qualified Sahasrara.Plugins.Netrunner.Type.Card as Card
import Sahasrara.Plugins.Netrunner.Type.CardCycle (CardCycle)
import qualified Sahasrara.Plugins.Netrunner.Type.CardCycle as Cycle
import Sahasrara.Plugins.Netrunner.Type.CardSet (CardSet)
import qualified Sahasrara.Plugins.Netrunner.Type.CardSet as CardSet
import qualified Sahasrara.Plugins.Netrunner.Type.Faction as Faction
import Sahasrara.Plugins.Netrunner.Type.Legality (Legality (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Printing (Printing (..))
import Sahasrara.Plugins.Netrunner.Utility.Card (formatCard, toFaction)
import Sahasrara.Plugins.Netrunner.Utility.Format
import Sahasrara.Plugins.Netrunner.Utility.Formatting (formatText)
import Sahasrara.Plugins.Netrunner.Utility.Legality (toLegality)
import Sahasrara.Utility
import Sahasrara.Utility.Types ()

toCard :: NrApi -> Printing -> Card
toCard api p = headNote "7" $ filter (\c -> Card.code c == cardCode p) $ cards api

-- | @toLink@ takes a printing and generates a link to its NetrunnerDB page.
toLink :: Printing -> Text
toLink printing = "https://netrunnerdb.com/en/card/" <> code printing

-- | @toCardSet@ takes a printing and finds its card set.
toCardSet :: NrApi -> Printing -> CardSet
toCardSet api printing =
  let setCode = cardSetCode printing
   in headNote "8" $ filter ((== setCode) . CardSet.code) $ cardSets api

-- | @toCycle@ takes a printing and finds its cycle.
toCycle :: NrApi -> Printing -> CardCycle
toCycle api printing =
  let cycleCode = CardSet.cycleCode $ toCardSet api printing
   in headNote "9" $ filter ((== cycleCode) . Cycle.code) $ cardCycles api

-- | @toFlavour@ gets a cards flavour text.
toFlavour :: Printing -> EnvDatabaseDiscord NrApi (Maybe Text)
toFlavour Printing {flavour = flavour} = case flavour of
  Nothing -> return Nothing
  Just f -> do
    f' <- formatText f
    return $ Just f'

-- | @toImage@ takes a printing and loads an embed image of it.
toImage :: Printing -> CreateEmbedImage
toImage = CreateEmbedImageUrl . image

formatPrinting :: Printing -> EnvDatabaseDiscord NrApi Text
formatPrinting printing = do
  api <- ask
  formatCard $ toCard api printing

-- | @toReleaseData@ checks if a card was released in a data pack or a big
-- box, and simplifies this info in the case of the latter.
toReleaseData :: NrApi -> Printing -> Text
toReleaseData api printing = fromMaybe "" helper
  where
    helper :: Maybe Text
    helper = do
      let card = toCard api printing
          cardSet = toCardSet api printing
          cardCycle = toCycle api printing
          faction = Faction.name $ toFaction api card
          legality = case toLegality api (toActiveSnapshot api $ getStandard api) $ toCard api printing of
            Legal -> ""
            Rotated -> " (rotated)"
            Invalid -> " (non-Standard)"
            Banned -> " (banned)"
            Restricted -> " (restricted)"
            UniversalFactionCost x -> " (+" <> intToText x <> " universal influence)"
            GlobalPenalty -> " (identity influence reduction)"
            Points x -> " (points: " <> intToText x <> ")"
          expansion =
            Cycle.name cardCycle <> legality
              <> if CardSet.name cardSet /= Cycle.name cardCycle
                then " • " <> CardSet.name cardSet
                else ""
          pos = " #" <> (intToText $ position printing)
      return $ faction <> " • " <> expansion <> pos
