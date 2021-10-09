-- |
-- Module      : Tablebot.Plugins.Quote
-- Description : A more complex example using databases.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which allows user to @!quote add@ their favourite
-- quotes and then @!quote show n@ a particular quote.
module Tablebot.Plugins.Quote
  ( quotePlugin,
  )
where

import Data.Text (Text (..), append, pack, unpack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int (Int64)
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessageVoid)
import Tablebot.Plugin.SmartCommand
import Text.Megaparsec

-- Our Quote table in the database. This is fairly standard for Persistent,
-- however you should note the name of the migration made.
share
  [mkPersist sqlSettings, mkMigrate "quoteMigration"]
  [persistLowerCase|
Quote
    quote String
    author String
    deriving Show
|]

-- | Our quote command, which combines @addQuote@ and @showQuote@.
quote :: Command
quote =
  Command
    "quote"
    (parseComm quoteComm)

quoteComm :: WithError "Unknown quote functionality." (Either (Exactly "add", Quoted, Exactly "-", RestOfInput) (Exactly "show", Int)) -> Message -> DatabaseDiscord ()
quoteComm (WErr (Left (_, Qu quote, _, ROI author))) = addQ (unpack quote) (unpack author)
quoteComm (WErr (Right (_, id))) = showQ (fromIntegral id)

-- | @addQuote@, which looks for a message of the form
-- @!quote add "quoted text" - author@, and then stores said quote in the
-- database, returning the ID used.
addQ :: String -> String -> Message -> DatabaseDiscord ()
addQ quote author m = do
  added <- insert $ Quote quote author
  let res = pack $ show $ fromSqlKey added
  sendMessageVoid m ("Quote added as #" `append` res)

-- | @showQuote@, which looks for a message of the form @!quote show n@, looks
-- that quote up in the database and responds with that quote.
showQ :: Int64 -> Message -> DatabaseDiscord ()
showQ id m = do
  qu <- get $ toSqlKey id
  case qu of
    Just (Quote txt author) ->
      sendMessageVoid m $ pack $ txt ++ " - " ++ author
    Nothing -> sendMessageVoid m "Couldn't get that quote!"

showQuoteHelp :: HelpPage
showQuoteHelp = HelpPage "show" "show a quote by number" "**Show Quote**\nShows a quote by id\n\n*Usage:* `quote show <id>`" []

addQuoteHelp :: HelpPage
addQuoteHelp = HelpPage "add" "add a new quote" "**Add Quote**\nAdds a quote\n\n*Usage:* `quote add \"quote\" - author`" []

quoteHelp :: HelpPage
quoteHelp = HelpPage "quote" "store and retrieve quotes" "**Quotes**\nAllows storing and retrieving quotes" [showQuoteHelp, addQuoteHelp]

-- | @quotePlugin@ assembles the @quote@ command (consisting of @add@ and
-- @show@) and the database migration into a plugin.
quotePlugin :: Plugin
quotePlugin = plug {commands = [quote], migrations = [quoteMigration], helpPages = [quoteHelp]}
