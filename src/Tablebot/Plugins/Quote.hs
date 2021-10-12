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

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (append, pack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int (Int64)
import System.Random (randomRIO)
import Tablebot.Plugin
import Tablebot.Plugin.Discord (Message, sendMessageVoid)
import Tablebot.Plugin.Permission (requirePermission)
import Tablebot.Plugin.SmartCommand
import Text.RawString.QQ

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

quoteComm ::
  WithError
    "Unknown quote functionality."
    ( Either
        ( Either
            (Exactly "add", Quoted String, Exactly "-", RestOfInput String)
            (Exactly "edit", Int, Quoted String, Exactly "-", RestOfInput String)
        )
        ( Either
            ( Either
                (Exactly "show", Int)
                (Exactly "delete", Int)
            )
            (Exactly "random")
        )
    ) ->
  Message ->
  DatabaseDiscord ()
quoteComm (WErr (Left (Left (_, Qu qu, _, ROI author)))) = addQ qu author
quoteComm (WErr (Left (Right (_, qId, Qu qu, _, ROI author)))) = editQ (fromIntegral qId) qu author
quoteComm (WErr (Right (Left (Left (_, qId))))) = showQ (fromIntegral qId)
quoteComm (WErr (Right (Left (Right (_, qId))))) = deleteQ (fromIntegral qId)
quoteComm (WErr (Right (Right (_)))) = randomQ

-- | @showQuote@, which looks for a message of the form @!quote show n@, looks
-- that quote up in the database and responds with that quote.
showQ :: Int64 -> Message -> DatabaseDiscord ()
showQ qId m = do
  qu <- get $ toSqlKey qId
  case qu of
    Just (Quote txt author) ->
      sendMessageVoid m $ pack $ txt ++ " - " ++ author
    Nothing -> sendMessageVoid m "Couldn't get that quote!"

-- | @randomQuote@, which looks for a message of the form @!quote show n@, looks
-- that quote up in the database and responds with that quote.
randomQ :: Message -> DatabaseDiscord ()
randomQ m = do
  num <- count allQuotes
  rindex <- liftIO $ randomRIO (0, (num - 1))
  key <- selectKeysList allQuotes [OffsetBy rindex, LimitTo 1]
  qu <- get $ head key
  case qu of
    Just (Quote txt author) ->
      sendMessageVoid m $ pack $ txt ++ " - " ++ author
    Nothing -> sendMessageVoid m "Couldn't find any quotes!"
  return ()
  where
    allQuotes :: [Filter Quote]
    allQuotes = []

-- | @addQuote@, which looks for a message of the form
-- @!quote add "quoted text" - author@, and then stores said quote in the
-- database, returning the ID used.
addQ :: String -> String -> Message -> DatabaseDiscord ()
addQ qu author m = do
  added <- insert $ Quote qu author
  let res = pack $ show $ fromSqlKey added
  sendMessageVoid m ("Quote added as #" `append` res)

-- | @addQuote@, which looks for a message of the form
-- @!quote add "quoted text" - author@, and then stores said quote in the
-- database, returning the ID used.
editQ :: Int64 -> String -> String -> Message -> DatabaseDiscord ()
editQ qId qu author m =
  requirePermission Any m $
    let k = toSqlKey qId
     in do
          oQu <- get k
          case oQu of
            Just (Quote _ _) -> do
              replace k $ Quote qu author
              sendMessageVoid m "Quote updated"
            Nothing -> sendMessageVoid m "Couldn't update that quote!"

-- | @deleteQuote@, which looks for a message of the form @!quote delete n@,
-- and removes it from the database.
deleteQ :: Int64 -> Message -> DatabaseDiscord ()
deleteQ qId m =
  requirePermission Any m $
    let k = toSqlKey qId
     in do
          qu <- get k
          case qu of
            Just (Quote _ _) -> do
              delete k
              sendMessageVoid m "Quote deleted"
            Nothing -> sendMessageVoid m "Couldn't delete that quote!"

showQuoteHelp :: HelpPage
showQuoteHelp = HelpPage "show" "show a quote by number" "**Show Quote**\nShows a quote by id\n\n*Usage:* `quote show <id>`" [] None

deleteQuoteHelp :: HelpPage
deleteQuoteHelp =
  HelpPage
    "delete"
    "delete a quote by number"
    [r|**Delete Quote**
Delete a quote by id
Requires moderation permission

*Usage:* `quote delete <id>`|]
    []
    Any

editQuoteHelp :: HelpPage
editQuoteHelp =
  HelpPage
    "delete"
    "delete a quote by number"
    [r|**Edit Quote**
Edit a quote by id
Requires moderation permission

*Usage:* `quote delete <id>`|]
    []
    Any

addQuoteHelp :: HelpPage
addQuoteHelp = HelpPage "add" "add a new quote" "**Add Quote**\nAdds a quote\n\n*Usage:* `quote add \"quote\" - author`" [] None

quoteHelp :: HelpPage
quoteHelp = HelpPage "quote" "store and retrieve quotes" "**Quotes**\nAllows storing and retrieving quotes" [showQuoteHelp, addQuoteHelp, editQuoteHelp, deleteQuoteHelp] None

-- | @quotePlugin@ assembles the @quote@ command (consisting of @add@ and
-- @show@) and the database migration into a plugin.
quotePlugin :: Plugin
quotePlugin = plug {commands = [quote], migrations = [quoteMigration], helpPages = [quoteHelp]}
