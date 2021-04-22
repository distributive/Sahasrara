{-|
Module      : Tablebot.Plugins.Quote
Description : A more complex example using databases.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

This is an example plugin which allows user to @!quote add@ their favourite
quotes and then @!quote show n@ a particular quote.
-}
module Tablebot.Plugins.Quote (
    quotePlugin
) where

import Tablebot.Plugin
import Tablebot.Plugin.Parser (quoted, sp, untilEnd, Parser)
import Tablebot.Plugin.Discord (sendMessageVoid, Message)

import Data.Text (append, pack)
import Text.Parsec
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int (Int64)

-- Our Quote table in the database. This is fairly standard for Persistent,
-- however you should note the name of the migration made.
share [mkPersist sqlSettings, mkMigrate "quoteMigration"] [persistLowerCase|
Quote
    quote String
    author String
    deriving Show
|]

-- | Our quote command, which combines @addQuote@ and @showQuote@.
quote :: Command
quote = Command "quote" (
    ((try (string "add") *> addQuote) <|> (try (string "show") *> showQuote))
        <?> "Unknown quote functionality.")

-- | @addQuote@, which looks for a message of the form
-- @!quote add "quoted text" - author@, and then stores said quote in the
-- database, returning the ID used.
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

-- | @showQuote@, which looks for a message of the form @!quote show n@, looks
-- that quote up in the database and responds with that quote.
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

-- | @quotePlugin@ assembles the @quote@ command (consisting of @add@ and
-- @show@) and the database migration into a plugin.
quotePlugin :: Plugin
quotePlugin = plug { commands = [quote], migrations = [quoteMigration] }