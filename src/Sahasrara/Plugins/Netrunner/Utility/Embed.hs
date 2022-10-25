{-# LANGUAGE DuplicateRecordFields #-}
-- |
-- Description : Functions for formatting Netrunner data into embeds.
--
module Sahasrara.Plugins.Netrunner.Utility.Embed
  ( printingToEmbed,
    printingToEmbedWithText,
    printingsToEmbed,
    printingToImgEmbed,
    printingToFlavourEmbed,
    embedText,
    embedTextWithUrl,
    embedColumns,
    embedColumnsWithUrl,
    emptyField,
    embedLines,
  )
where

import Data.List.Split (splitPlaces)
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T (length)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Card (Card (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Type.Printing (Printing (..))
import Sahasrara.Plugins.Netrunner.Utility.Card
import Sahasrara.Plugins.Netrunner.Utility.Faction
import Sahasrara.Plugins.Netrunner.Utility.Printing
import Sahasrara.Utility
import Sahasrara.Utility.Embed (addColour)
import Sahasrara.Utility.Types ()
import Prelude hiding (unwords)

-- | @printingToEmbed@ takes a printing and creates an embed representation of it.
printingToEmbed :: NrApi -> Printing -> EnvDatabaseDiscord NrApi CreateEmbed
printingToEmbed api printing = do
  let card = toCard api printing
      eTitle = title card
      eURL = toLink printing
      eFoot = toReleaseData api printing
      eImg = Just $ toImage printing
      eColour = toFactionColour api card
  eText <- formatPrinting printing
  return $ addColour eColour $ CreateEmbed "" "" Nothing eTitle eURL eImg eText [] Nothing eFoot Nothing Nothing Nothing

-- | @printingToEmbedWithText@ embeds text and decorates it with a given printing.
printingToEmbedWithText :: NrApi -> Printing -> Text -> EnvDatabaseDiscord NrApi CreateEmbed
printingToEmbedWithText api printing text = do
  let card = toCard api printing
      eTitle = title card
      eURL = toLink printing
      eImg = Just $ toImage printing
      eColour = toFactionColour api card
  return $ addColour eColour $ CreateEmbed "" "" Nothing eTitle eURL eImg text [] Nothing "" Nothing Nothing Nothing

-- | @printingsToEmbed@ takes a list of printings and embeds their names with links.
printingsToEmbed :: NrApi -> Text -> [Printing] -> Text -> Text -> EnvDatabaseDiscord NrApi CreateEmbed
printingsToEmbed api pre printings post err = do
  formatted <- mapM formatPrintingText $ take 10 printings
  let ps = "**" <> intercalate "\n" formatted <> "**"
      eTitle = ":mag_right: **" <> pack (show $ length printings) <> " results**"
      eText = pre <> "\n" <> ps <> "\n" <> if length printings > 10 then err else post
  return $ CreateEmbed "" "" Nothing eTitle "" Nothing eText [] Nothing "" Nothing Nothing Nothing
  where
    formatPrintingText :: Printing -> EnvDatabaseDiscord NrApi Text
    formatPrintingText printing = do
      let card = toCard api printing
          title' = title card
          link = toLink printing
      icon <- toEmoji $ toFaction api card
      return $ icon <> " [" <> title' <> "](" <> link <> ")"

-- | @printingToImgEmbed@ takes a printing and embeds a picture of it.
printingToImgEmbed :: NrApi -> Printing -> CreateEmbed
printingToImgEmbed api printing =
  let card = toCard api printing
      eTitle = title card
      eURL = toLink printing
      eColour = toColour $ toFaction api card
      eImg = Just $ toImage printing
   in addColour eColour $ CreateEmbed "" "" Nothing eTitle eURL Nothing "" [] eImg "" Nothing Nothing Nothing

-- | @printingToFlavourEmbed@ takes a printing and attempts to embed its flavour text.
printingToFlavourEmbed :: NrApi -> Printing -> EnvDatabaseDiscord NrApi CreateEmbed
printingToFlavourEmbed api printing = do
  let card = toCard api printing
      eTitle = title card
      eURL = toLink printing
      eColour = toFactionColour api card
      eImg = Just $ toImage printing
      fallback = CreateEmbed "" "" Nothing eTitle eURL eImg "`Card has no flavour text`" [] Nothing "" Nothing Nothing Nothing
  flavor <- toFlavour printing
  return $
    addColour eColour $ case flavor of
      Nothing -> fallback
      Just "" -> fallback
      Just eFlavour -> CreateEmbed "" "" Nothing eTitle eURL eImg eFlavour [] Nothing "" Nothing Nothing Nothing

-- | @embedText@ just embeds the given text.
embedText :: Text -> Text -> CreateEmbed
embedText title text = CreateEmbed "" "" Nothing title "" Nothing text [] Nothing "" Nothing Nothing Nothing

-- | @embedTextWithUrl@ is @embedText@ but you can set the title URL.
embedTextWithUrl :: Text -> Text -> Text -> CreateEmbed
embedTextWithUrl title url text = CreateEmbed "" "" Nothing title url Nothing text [] Nothing "" Nothing Nothing Nothing

-- | @embedColumns@ embeds Text into columns.
embedColumns :: Text -> Text -> [(Text, [Text])] -> CreateEmbed
embedColumns title = embedColumnsWithUrl title ""

-- | @embedColumns@ embeds Text into columns with a linked title URL.
embedColumnsWithUrl :: Text -> Text -> Text -> [(Text, [Text])] -> CreateEmbed
embedColumnsWithUrl title url pre cols =
  let fields = map (\x -> EmbedField (fst x) (intercalate "\n" $ snd x) $ Just True) cols
   in CreateEmbed "" "" Nothing title url Nothing pre fields Nothing "" Nothing Nothing Nothing

-- | @emptyField@ represents an empty field for use in embedColumns.
-- NOTE: fields can't be empty, so it uses italicised spaces to force blankness
emptyField :: (Text, [Text])
emptyField = ("_ _", ["_ _"])

-- | @embedLines@ embeds a list of lines, splitting them into columns as needed.
-- NOTE: does not preserve order
embedLines :: Text -> Text -> [Text] -> CreateEmbed
embedLines title pre xs =
  let cumLength = scanl (\l x -> 1 + T.length x + l) (T.length title + 2) xs -- +1 for each newline title characters
      safeIndex = length $ takeWhile (< 1900) cumLength -- 1900 instead of 2000 because I gave up trying to be exact
      xs' = take safeIndex xs
      c = if length xs' < 12 then 1 else 2 --if length xs' < 27 then 2 else 3
      d = length xs' `div` c
      m = length xs' `mod` c
      heights = replicate m (d + 1) ++ replicate (c - m) d
      cols = splitPlaces heights xs'
      fields = map (\x -> EmbedField "⠀" (intercalate "\n" x) $ Just True) cols
   in CreateEmbed "" "" Nothing title "" Nothing pre fields Nothing "" Nothing Nothing Nothing
