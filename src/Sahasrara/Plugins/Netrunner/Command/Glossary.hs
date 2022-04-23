-- |
-- Module      : Sahasrara.Plugins.Netrunner.Command.Glossary
-- Description : A command that flips a coin, or randomly selects from a list.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- A command for dismystifying the language of Netrunner.
module Sahasrara.Plugins.Netrunner.Command.Glossary (nrGlossary) where

import Data.Text (Text, intercalate, unpack, pack)
import Sahasrara.Utility hiding (name)
import Sahasrara.Utility.Discord (Message, sendEmbedMessage)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))
import Text.RawString.QQ
import Sahasrara.Utility.Embed (basicEmbed)
import Sahasrara.Plugins.Netrunner.Type.Glossary (Definition (..))
import Data.Map (fromList, lookup)
import Prelude hiding (lookup)
import Control.Monad.Reader (ask)
import Sahasrara.Utility.Search (FuzzyCosts (..), closestMatchWithCosts)
import Sahasrara.Plugins.Netrunner.Type.NrApi

-- | @glossary@ looks up a given term, or shows the full list if none are
-- provided.
nrGlossary :: EnvCommand NrApi
nrGlossary = Command "glossary" (parseComm glossaryComm) []
  where
    glossaryComm ::
      Either () (RestOfInput Text) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    glossaryComm (Left ()) = printAll
    glossaryComm (Right (ROI arg)) = printDef arg

-- | @printAll@ sends a message displaying all definitions in the glossary
printAll :: Message -> EnvDatabaseDiscord NrApi ()
printAll m = do
  api <- ask
  sendEmbedMessage m "" $ basicEmbed ":pencil: Glossary" $ intercalate "\n" $ map formatDef $ glossary api
  where
    formatDef :: Definition -> Text
    formatDef Definition { name = name, aliases = aliases, short = short } =
      let left = "**" <> name <> "**" <> case length aliases of
                                           0 -> ": "
                                           _ -> " [`" <> (intercalate "`|`" aliases) <> "`]: "
       in left <> "*" <> short <> "*"

-- | @printDef@ attempts to find a specific term in the glossary
printDef :: Text -> Message -> EnvDatabaseDiscord NrApi ()
printDef term m = do
  api <- ask
  let defMap = fromList $ concatMap (\def -> (name def, def) : [(a,def) | a <- aliases def]) $ glossary api
      suggestion = pack $ closestMatchWithCosts editCosts (map (unpack . name) $ glossary api) $ unpack term
      failText = "Did you mean: `" <> suggestion <> "`?"
  case lookup term defMap of
    Nothing -> sendEmbedMessage m "" $ basicEmbed ":pencil2: Term not found" failText
    Just def -> sendEmbedMessage m "" $ basicEmbed (":pencil: " <> name def) $ format def
  where
    editCosts :: FuzzyCosts
    editCosts = FuzzyCosts
      { deletion = 5,
        insertion = 0,
        substitution = 10,
        transposition = 1
      }
    format :: Definition -> Text
    format Definition {long = long, related = related} =
      let suffix = case related of
                     [] -> ""
                     _ -> "**See also**\n`" <> intercalate "`, `" related <> "`"
       in long <> "\n\n" <> suffix
