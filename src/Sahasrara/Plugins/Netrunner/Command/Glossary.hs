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
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, intercalate, unpack)
import Sahasrara.Plugins.Netrunner.Type.Card (title)
import Sahasrara.Plugins.Netrunner.Type.Glossary (Definition (..), Glossary (..))
import Sahasrara.Plugins.Netrunner.Type.NrApi
import Sahasrara.Plugins.Netrunner.Utility.Alias (fromAlias)
import Sahasrara.Plugins.Netrunner.Utility.Format (formatText')
import Sahasrara.Utility hiding (name)
import Sahasrara.Utility.Discord (Message, sendEmbedMessage)
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
      disclaimer = "\n\n*This glossary is supported by members of the community. It is not endorsed by NISEI, nor is the content guaranteed to be consistent with the game's official rules.*"
  randomDef <- liftIO $ chooseOne $ map name definitions
  let example = "\nTry: `glossary " <> randomDef <> "`"
  sendEmbedMessage m "" $ addColour Blue $ basicEmbed ":pencil: Glossary" $ desc <> example <> attribution <> disclaimer

-- | @printDef@ attempts to find a specific term in the glossary
printDef :: Text -> Message -> EnvDatabaseDiscord NrApi ()
printDef term m = do
  api <- ask
  let g = glossary api
  case lookup (standardise term) $ fromList $ defMap $ defs g of
    Nothing -> sendEmbedMessage m "" $ addColour Red $ basicEmbed ":pencil2: Term not found" $ failText api
    Just def -> do
      let citation = case citations def of
            Just (x : xs) -> "\n\n***Written by:** " <> intercalate ", " (x : xs) <> "*"
            _ -> ""
          bib = case sources def of
            Just [x] -> "\n\n***Source:** " <> x <> "*"
            Just xs -> "\n\n***Sources:** " <> intercalate ", " xs <> "*"
            Nothing -> ""
      defText <- formatText' $ long def
      sendEmbedMessage m "" $ addColour Blue $ basicEmbed (":pencil: " <> name def) $ format defText (related def) citation bib (source g)
  where
    defMap :: [Definition] -> [(Text, Definition)]
    defMap definitions = concatMap (\def -> (standardise $ name def, def) : [(standardise $ a, def) | a <- aliases def]) definitions
    format :: Text -> [Text] -> Text -> Text -> Text -> Text
    format defText related citation bib source =
      let suffix = case related of
            [] -> ""
            _ -> "\n\n**See also**\n`" <> intercalate "`, `" related <> "`"
          footnote = "\n\n*Is this definition inaccurate, incomplete, or misleading? [Let me know!](" <> source <> ")*"
       in defText <> citation <> bib <> footnote <> suffix
    failText :: NrApi -> Text
    failText NrApi {cards = cards, glossary = glossary, cardAliases = cardAliases} =
      let definitions = map (\(s, d) -> (unpack s, d)) $ defMap $ defs $ glossary
          suggestions = map formatDef $ take 3 $ nub $ sortValuesWithCosts editCosts definitions $ unpack term
          footnote = "\n\n*Can't find a definition you think should be here? [Let me know!](" <> source glossary <> ")*"
       in if fromAlias cardAliases term /= term
            then "That's the alias of a card; try: `[[" <> (fromAlias cardAliases term) <> "]]`"
            else case filter ((== Just term) . title) cards of
              [] -> "Maybe you meant:\n " <> intercalate "\n " suggestions <> footnote
              c : _ -> "That's a card; try: `[[" <> (fromMaybe "" $ title c) <> "]]`"
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
