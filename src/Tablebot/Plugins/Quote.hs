module Tablebot.Plugins.Quote (
    quotePlugin
) where

import Tablebot.Plugin
import Tablebot.Plugin.Parser (skipSpace, Parser)
import Tablebot.Plugin.Discord (sendMessageVoid, Message)

import Data.Text (append, pack)
import Text.Parsec
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int (Int64)

share [mkPersist sqlSettings, mkMigrate "quoteMigration"] [persistLowerCase|
Quote
    quote String
    author String
    deriving Show
|]

addQuote :: Command
addQuote = Command "addquote" parser
  where parser :: Parser (Message -> DatabaseDiscord ())
        parser = do
            skipSpace
            quote <- between (char '"' <?> "Couldn't find opening quote.")
                (char '"' <?> "Couldn't find closing quote.")
                (many1 $ noneOf ['"']) <?> "Couldn't get quote!"
            skipSpace
            author <- manyTill anyChar eof
            return $ addQ quote author
        addQ :: String -> String -> Message -> DatabaseDiscord ()
        addQ quote author m = do
            added <- insert $ Quote quote author
            let res = pack $ show $ fromSqlKey added
            sendMessageVoid m ("Quote added as #" `append` res)

showQuote :: Command
showQuote = Command "showquote" parser
  where parser :: Parser (Message -> DatabaseDiscord ())
        parser = do
            skipSpace
            num <- many1 digit
            let id = read num :: Int64
            return $ showQ id
        showQ :: Int64 -> Message -> DatabaseDiscord ()
        showQ id m = do
            qu <- get $ toSqlKey id
            case qu of
                Just (Quote txt author) ->
                    sendMessageVoid m $ pack $ txt ++ " - " ++ author
                Nothing -> sendMessageVoid m "Couldn't get that quote!"

quotePlugin :: Plugin
quotePlugin = plug { commands = [addQuote, showQuote], migrations = [quoteMigration] }