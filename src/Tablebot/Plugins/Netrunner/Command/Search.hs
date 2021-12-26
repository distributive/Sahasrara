{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Tablebot.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the search command.
module Tablebot.Plugins.Netrunner.Command.Search
  ( Query,
    QueryComp,
    searchCards,
    fixSearch,
    pairsToQuery,
    pairsToNrdb,
  )
where

import Data.List (nubBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, intercalate, isInfixOf, pack, replace, toLower, unpack, unwords)
import Tablebot.Plugins.Netrunner.Type.BanList (BanList)
import qualified Tablebot.Plugins.Netrunner.Type.BanList as BanList
import Tablebot.Plugins.Netrunner.Type.Card as Card
import qualified Tablebot.Plugins.Netrunner.Type.Cycle as Cycle
import qualified Tablebot.Plugins.Netrunner.Type.Faction as Faction
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi (..))
import qualified Tablebot.Plugins.Netrunner.Type.Type as Type
import Tablebot.Plugins.Netrunner.Utility.BanList (activeBanList, isBanned, latestBanList)
import Tablebot.Plugins.Netrunner.Utility.Card (toCycle)
import Tablebot.Utility.Search (autocomplete, closestMatch, closestPair)
import Tablebot.Utility.Types ()
import Tablebot.Utility.Utils (standardise)
import Text.Read (readMaybe)
import Prelude hiding (unwords)

-- | @Query@ represents a single search query with its arguments.
data Query
  = QText Text QueryComp (Card -> Maybe Text) [Text]
  | QInt Text QueryComp (Card -> Maybe Int) [Text]
  | QBool Text QueryComp (Card -> Maybe Bool) [Text] -- TODO: make this a single Text?
  | QBan Text QueryComp (Text, BanList)

-- | @QueryComp@ represents the types of comparison queries might take
data QueryComp = QEQ | QNE | QGT | QLT

-- | @searchCards@ looks for all cards that match a set of criteria.
searchCards :: NrApi -> [Query] -> Maybe [Card]
searchCards _ [] = Nothing
searchCards api pairs = Just $ nubBy cardEq $ foldr filterCards (cards api) pairs
  where
    cardEq :: Card -> Card -> Bool
    cardEq a b = title a == title b
    filterCards :: Query -> [Card] -> [Card]
    filterCards (QText _ sep f xs) = filterText sep f xs
    filterCards (QInt _ sep f xs) = filterInt sep f xs
    filterCards (QBool _ sep f xs) = filterBool sep f xs
    filterCards (QBan _ sep x) = filterBan sep x
    filterText :: QueryComp -> (Card -> Maybe Text) -> [Text] -> ([Card] -> [Card])
    filterText sep f xs =
      let check c x = isInfixOf (standardise x) $ standardise $ fromMaybe "" $ f c
          fil c = any (check c) xs
       in case sep of
            QEQ -> filter fil
            QNE -> filter (not . fil)
            _ -> id
    filterInt :: QueryComp -> (Card -> Maybe Int) -> [Text] -> ([Card] -> [Card])
    filterInt sep f xs =
      let check c x = fromMaybe False $ do
            a <- readMaybe $ unpack x
            b <- f c
            return $ (compFunc sep) a b
          fil c = any (check c) xs
       in filter fil
    compFunc :: QueryComp -> Int -> Int -> Bool
    compFunc QEQ = (==)
    compFunc QNE = (/=)
    compFunc QGT = (<)
    compFunc QLT = (>)
    filterBool :: QueryComp -> (Card -> Maybe Bool) -> a -> ([Card] -> [Card])
    filterBool QEQ f _ = filter (fromMaybe False . f)
    filterBool QNE f _ = filter (maybe True not . f)
    filterBool _ _ _ = id
    filterBan :: QueryComp -> (Text, BanList) -> ([Card] -> [Card])
    filterBan _ (_, b) = filter (not . (isBanned b))

-- | @fixSearch@ takes a list of key/value pairs, formats them, and
-- repairs damaged queries to ensure they are valid for NetrunnerDB.
fixSearch :: NrApi -> [(String, Char, [String])] -> [Query]
fixSearch api = mapMaybe fix
  where
    fix :: (String, Char, [String]) -> Maybe (Query)
    fix pair = do
      a <- setComp pair
      b <- format $ packValues a
      c <- checkComp b
      return c
    setComp :: (String, Char, [String]) -> Maybe (String, QueryComp, [String])
    setComp (k, ':', v) = Just (k, QEQ, v)
    setComp (k, '!', v) = Just (k, QNE, v)
    setComp (k, '>', v) = Just (k, QGT, v)
    setComp (k, '<', v) = Just (k, QLT, v)
    setComp _ = Nothing
    packValues :: (String, QueryComp, [String]) -> (String, QueryComp, [Text])
    packValues (k, sep, v) = (k, sep, map pack v)
    format :: (String, QueryComp, [Text]) -> Maybe (Query)
    format ("x", sep, v) = Just $ QText "x" sep strippedText v
    format ("a", sep, v) = Just $ QText "a" sep flavour v
    format ("e", sep, v) = Just $ QText "e" sep packCode v
    format ("c", sep, v) = Just $ QText "c" sep (\c -> Cycle.name <$> toCycle api c) v
    format ("t", sep, v) = Just $ QText "t" sep typeCode $ map fixType v
    format ("f", sep, v) = Just $ QText "f" sep keywords $ map fixFaction v
    format ("d", sep, v) = Just $ QText "d" sep sideCode v
    format ("i", sep, v) = Just $ QText "i" sep illustrator v
    format ("o", sep, v) = Just $ QInt "o" sep cost v
    format ("g", sep, v) = Just $ QInt "g" sep advancementCost v
    format ("m", sep, v) = Just $ QInt "m" sep memoryCost v
    format ("n", sep, v) = Just $ QInt "n" sep factionCost v
    format ("p", sep, v) = Just $ QInt "p" sep strength v
    format ("v", sep, v) = Just $ QInt "v" sep agendaPoints v
    format ("h", sep, v) = Just $ QInt "h" sep trashCost v
    -- format ("r", sep, v) =
    format ("u", sep, v) = Just $ QBool "u" sep uniqueness v
    format ("b", _, []) = Nothing
    format ("b", sep, v) = Just $ QBan "b" sep $ fixBan $ head v
    -- format ("z", sep, v) =
    format _ = Nothing
    fixFaction f = case autocomplete fNames f of
      Just f' -> f'
      Nothing -> pack $ closestMatch (map unpack fNames) $ unpack f
    fixType t = case autocomplete tNames t of
      Just t' -> t'
      Nothing -> pack $ closestMatch (map unpack tNames) $ unpack t
    fixBan :: Text -> (Text, BanList)
    fixBan b =
      let bls = banLists api
          active = ("active", activeBanList api)
          latest = ("latest", latestBanList api)
          blsPairs = active : latest : (zip (map (unpack . BanList.name) bls) bls)
       in (\(x, y) -> (pack x, y)) $ closestPair blsPairs $ unpack b
    fNames = map Faction.code $ factions api
    tNames = map (toLower . Type.name) $ filter (not . Type.is_subtype) $ types api
    checkComp :: Query -> Maybe Query
    checkComp (QText _ QGT _ _) = Nothing
    checkComp (QText _ QLT _ _) = Nothing
    checkComp (QText k sep f s) = Just $ QText k sep f s
    checkComp (QInt k QGT f s) = if length s == 1 then Just (QInt k QGT f s) else Nothing
    checkComp (QInt k QLT f s) = if length s == 1 then Just (QInt k QLT f s) else Nothing
    checkComp (QInt k sep f s) = Just $ QInt k sep f s
    checkComp (QBool _ QGT _ _) = Nothing
    checkComp (QBool _ QLT _ _) = Nothing
    checkComp (QBool k sep f s) = Just $ QBool k sep f s
    -- NRDB allows QNE but it's functionally the same as QEQ
    -- Also note that for type reasons the length of QBans' arguments are fixed in format
    checkComp (QBan k _ s) = Just $ QBan k QEQ s

-- | @pairsToQuery@ takes a set of search query pairs ands turns it into a link
-- to an equivalent search on NetrunnerDB.
pairsToQuery :: [Query] -> Text
pairsToQuery pairs = "<https://netrunnerdb.com/find/?q=" <> replace " " "+" (pairsToNrdb pairs) <> ">"

-- | @pairsToNrdb@ takes a set of search query pairs and formats it into a valid
-- plaintext search query for NetrunnerDB.
pairsToNrdb :: [Query] -> Text
pairsToNrdb pairs = unwords queries
  where
    queries :: [Text]
    queries = map format pairs
    format :: Query -> Text
    format (QText k sep _ vs) = format' k sep vs
    format (QInt k sep _ vs) = format' k sep vs
    format (QBool k sep _ vs) = format' k sep vs
    format (QBan k sep v) = format' k sep [fst v]
    format' :: Text -> QueryComp -> [Text] -> Text
    format' k sep vs =
      let v = intercalate "|" $ map formatValue vs
       in k <> fromComp sep <> v
    formatValue :: Text -> Text
    formatValue v =
      if " " `isInfixOf` v
        then "\"" <> v <> "\""
        else v
    fromComp :: QueryComp -> Text
    fromComp QEQ = ":"
    fromComp QNE = "!"
    fromComp QGT = ">"
    fromComp QLT = "<"
