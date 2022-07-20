{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Sahasrara.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the horoscope command.
module Sahasrara.Plugins.Netrunner.Command.Horoscope (nrHoroscope) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (mapMaybe)
import Data.Text (Text, isInfixOf)
import Data.Text.ICU.Replace (replaceAll)
import Data.Time.Calendar
import Data.Time.Clock
import Discord.Types
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Type.Blacklist (Blacklist (..))
import Sahasrara.Plugins.Netrunner.Type.Card (Card (flavour, title))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.Embed
import Sahasrara.Plugins.Netrunner.Utility.Format (formatText)
import Sahasrara.Utility
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (sendEmbedMessage)
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Random (chooseOneSeeded)
import Sahasrara.Utility.Types ()
import Text.RawString.QQ (r)

-- | @nrHoroscope@ gets a random piece of flavour text from the card pool,
-- seeded by the current date.
nrHoroscope :: EnvCommand NrApi
nrHoroscope = Command "horoscope" horoscopePars []
  where
    horoscopePars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    horoscopePars = return $ \m -> do
      api <- ask
      let fs = filterFlavours (blacklist api) (cards api)
      seed <- liftIO $ getCurrentTime >>= return . fromIntegral . toModifiedJulianDay . utctDay
      f <- liftIO $ chooseOneSeeded seed fs
      f' <- formatText f
      sendEmbedMessage m "" $ addColour colHoroscope $ embedText ":crystal_ball: Horoscope :crystal_ball:" $ replaceAll [r|"(.*?)"[.\S\s]*|] "$1" f'
    filterFlavours :: Blacklist -> [Card] -> [Text]
    filterFlavours Blacklist {badSubstrings = badSubstrings, badCards = badCards} cards =
      let flavoured = filter ((Nothing /=) . flavour) cards
          withoutBadCards = filter (\c -> all (\b -> Just b /= title c) badCards) flavoured
       in filter (\c -> not $ any (`isInfixOf` c) badSubstrings) $ mapMaybe flavour withoutBadCards -- Without bad substrings
