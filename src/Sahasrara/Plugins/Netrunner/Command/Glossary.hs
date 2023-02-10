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

import Control.Monad.Reader (ask, liftIO)
import Data.List (nub)
import Data.Map (fromList, lookup)
import Data.Maybe (catMaybes)
import Data.Text (Text, intercalate, unpack)
import Discord.Types
import Sahasrara.Plugins.Netrunner.Type.Card (title)
import Sahasrara.Plugins.Netrunner.Type.Glossary (Definition (..), Glossary (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi
import Sahasrara.Plugins.Netrunner.Utility.Alias (fromAlias)
import Sahasrara.Plugins.Netrunner.Utility.Formatting (formatText')
import Sahasrara.Utility
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (sendEmbedMessage)
import Sahasrara.Utility.Embed (addColour, basicEmbed)
import Sahasrara.Utility.Random (chooseOne)
import Sahasrara.Utility.Search (FuzzyCosts (..), sortValuesWithCosts)
import Sahasrara.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))
import Prelude hiding (lookup)

-- | @glossary@ looks up a given term, or shows the full list if none are
-- provided.
nrGlossary :: EnvCommand NrApi
nrGlossary = Command "glossary" (parseComm glossaryComm) []
  where
    glossaryComm ::
      Either () (RestOfInput Text) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    glossaryComm (Left ()) = printMain
    glossaryComm (Right (ROI arg)) = printDef arg

-- | @printMain@ sends a message displaying all definitions in the glossary
printMain :: Message -> EnvDatabaseDiscord NrApi ()
printMain m = do
  api <- ask
  let definitions = defs $ glossary api
      desc = "This command allows you to search a glossary of Netrunner terminology."
      attribution = case authors of
        [] -> ""
        _ -> "\n\n**With thanks to:**\n• " <> intercalate "\n• " authors
      authors = nub $ concat $ catMaybes $ citations <$> definitions
      disclaimer = "\n\n*This glossary is supported by members of the community. It is not endorsed by Null Signal Games, nor is the content guaranteed to be consistent with the game's official rules.*"
  randomDef <- liftIO $ chooseOne $ map name definitions
  let example = "\nTry: `glossary " <> randomDef <> "`"
  sendEmbedMessage m "" $ addColour colHelp $ basicEmbed ":pencil: Glossary" $ desc <> example <> attribution <> disclaimer

-- | @printDef@ attempts to find a specific term in the glossary
printDef :: Text -> Message -> EnvDatabaseDiscord NrApi ()
printDef term m = do
  api <- ask
  let g = glossary api
  case lookup (standardise term) $ fromList $ defMap $ defs g of
    Nothing -> sendEmbedMessage m "" $ addColour colError $ basicEmbed ":pencil2: Term not found" $ failText api
    Just def -> do
      let defCitation = case filter (\c -> not $ elem c $ hiddenCitations g) <$> citations def of
            Nothing -> ""
            Just [] -> ""
            Just [x] -> "\n\n***Citation:** " <> x <> "*"
            Just xs -> "\n\n***Citations:** " <> intercalate ", " xs <> "*"
          defSource = case sources def of
            Nothing -> ""
            Just [] -> ""
            Just [x] -> "\n\n***Source:** " <> x <> "*"
            Just xs -> "\n\n***Sources:** " <> intercalate ", " xs <> "*"
      defText <- formatText' $ long def
      sendEmbedMessage m "" $ addColour colInfo $ basicEmbed (":pencil: " <> name def) $ format defText (related def) defCitation defSource (source g)
  where
    defMap :: [Definition] -> [(Text, Definition)]
    defMap definitions = concatMap (\def -> (standardise $ name def, def) : [(standardise $ a, def) | a <- aliases def]) definitions
    format :: Text -> [Text] -> Text -> Text -> Text -> Text
    format defText related defCitation defSource source =
      let suffix = case related of
            [] -> ""
            _ -> "\n\n**See also**\n`" <> intercalate "`, `" related <> "`"
          footnote = "\n\n*Is this definition inaccurate, incomplete, or misleading? [Let me know!](" <> source <> ")*"
       in defText <> defCitation <> defSource <> footnote <> suffix
    failText :: NrApi -> Text
    failText NrApi {cards = cards, glossary = glossary, cardAliases = cardAliases} =
      let definitions = map (\(s, d) -> (unpack s, d)) $ defMap $ defs $ glossary
          suggestions = map formatDef $ take 3 $ nub $ sortValuesWithCosts editCosts definitions $ unpack term
          footnote = "\n\n*Can't find a definition you think should be here? [Let me know!](" <> source glossary <> ")*"
       in if fromAlias cardAliases term /= term
            then "That's the alias of a card; try: `[[" <> (fromAlias cardAliases term) <> "]]`"
            else case filter ((== term) . title) cards of
              [] -> "Maybe you meant:\n " <> intercalate "\n " suggestions <> footnote
              c : _ -> "That's a card; try: `[[" <> (title c) <> "]]`"
    formatDef :: Definition -> Text
    formatDef Definition {name = name, short = short} = "`" <> name <> "`: *" <> short <> "*"
    editCosts :: FuzzyCosts
    editCosts =
      FuzzyCosts
        { deletion = 5,
          insertion = 0,
          substitution = 10,
          transposition = 1
        }
