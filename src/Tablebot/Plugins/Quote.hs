module Tablebot.Plugins.Quote (
    quotePlugin
) where

import Tablebot.Plugin
import Tablebot.Plugin.Parser
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

quote :: Command
quote = Command "quote" (
    ((try (string "add") *> addQuote) <|> (try (string "show") *> showQuote))
        <?> "Unknown quote functionality.")

addQuote :: Parser (Message -> DatabaseDiscord ())
addQuote = do
    sp 
    quote <- try quoted <?> error ++ " (needed a quote)"
    sp
    char '-' <?> error ++ " (needed hyphen)"
    sp
    addQ quote <$> untilEnd <?> error ++ " (needed author)"
  where addQ :: String -> String -> Message -> DatabaseDiscord ()
        addQ quote author m = do
            added <- insert $ Quote quote author
            let res = pack $ show $ fromSqlKey added
            sendMessageVoid m ("Quote added as #" `append` res)
        error = "Syntax is: .quote add \"quote\" - author"

showQuote :: Parser (Message -> DatabaseDiscord ())
showQuote = do
    sp
    num <- many1 digit <?> error
    let id = read num :: Int64
    return $ showQ id
  where showQ :: Int64 -> Message -> DatabaseDiscord ()
        showQ id m = do
            qu <- get $ toSqlKey id
            case qu of
                Just (Quote txt author) ->
                    sendMessageVoid m $ pack $ txt ++ " - " ++ author
                Nothing -> sendMessageVoid m "Couldn't get that quote!"
        error = "Syntax is: .quote show n"

quotePlugin :: Plugin
quotePlugin = plug { commands = [quote], migrations = [quoteMigration] }