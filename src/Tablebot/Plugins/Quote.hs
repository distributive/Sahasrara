-- |
-- Module      : Tablebot.Plugins.Quote
-- Description : A more complex example using databases.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
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
import Data.Text (Text, append, pack, unpack)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Discord.Internal.Types.Events (ReactionInfo (reactionChannelId, reactionEmoji, reactionMessageId, reactionUserId))
import Discord.Types (Emoji (emojiName), Message (messageAuthor, messageChannel, messageId, messageText), UTCTime, messageGuild, userIsBot)
import GHC.Int (Int64)
import System.Random (randomRIO)
import Tablebot.Plugin
import Tablebot.Plugin.Discord (getMessage, getMessageLink, getPrecedingMessage, getReplyMessage, sendEmbedMessage, sendMessage, toMentionStr)
import Tablebot.Plugin.Embed
import Tablebot.Plugin.Permission (requirePermission)
import Tablebot.Plugin.SmartCommand
import Text.RawString.QQ (r)

-- Our Quote table in the database. This is fairly standard for Persistent,
-- however you should note the name of the migration made.
share
  [mkPersist sqlSettings, mkMigrate "quoteMigration"]
  [persistLowerCase|
Quote
    quote String
    author String
    msgId Int Maybe
    cnlId Int Maybe
    time UTCTime
    deriving Show
|]

-- | @quoteReactionAdd@ creates an event for when a reaction is added to a message.
-- If the reaction is :speech_balloon:, the message is added as a quote
quoteReactionAdd :: ReactionAdd
quoteReactionAdd = ReactionAdd qra
  where
    qra ri
      | emojiName (reactionEmoji ri) == "\x1F4AC" = do
        m <- m'
        case m of
          Left _ -> pure ()
          Right mes -> addMessageQuote mes mes
      | otherwise = return ()
      where
        m' = getMessage (reactionChannelId ri) (reactionMessageId ri)

-- | Our quote command, which combines various functions to create, display and update quotes.
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
            ( Either
                (Exactly "add", Quoted String, Exactly "-", RestOfInput String)
                (Exactly "edit", Int, Quoted String, Exactly "-", RestOfInput String)
            )
            ( Either
                (Exactly "this")
                (Exactly "user", RestOfInput String)
            )
        )
        ( Either
            ( Either
                (Exactly "show", Int)
                (Exactly "delete", Int)
            )
            ( Either
                (Exactly "random")
                ()
            )
        )
    ) ->
  Message ->
  DatabaseDiscord ()
quoteComm (WErr (Left (Left (Left (_, Qu qu, _, ROI author))))) = addQ qu author
quoteComm (WErr (Left (Left (Right (_, qId, Qu qu, _, ROI author))))) = editQ (fromIntegral qId) qu author
quoteComm (WErr (Left (Right (Left (_))))) = thisQ
quoteComm (WErr (Left (Right (Right (_, ROI author))))) = authorQ author
quoteComm (WErr (Right (Left (Left (_, qId))))) = showQ (fromIntegral qId)
quoteComm (WErr (Right (Left (Right (_, qId))))) = deleteQ (fromIntegral qId)
quoteComm (WErr (Right (Right (_)))) = randomQ

-- | @showQuote@, which looks for a message of the form @!quote show n@, looks
-- that quote up in the database and responds with that quote.
showQ :: Int64 -> Message -> DatabaseDiscord ()
showQ qId m = do
  qu <- get $ toSqlKey qId
  case qu of
    Just q -> renderQuoteMessage q qId m
    Nothing -> sendMessage m "Couldn't get that quote!"

-- | @randomQuote@, which looks for a message of the form @!quote random@,
-- selects a random quote from the database and responds with that quote.
randomQ :: Message -> DatabaseDiscord ()
randomQ m = do
  num <- count allQuotes
  if num == 0
    then sendMessage m "Couldn't find any quotes!"
    else do
      rindex <- liftIO $ randomRIO (0, (num - 1))
      key <- selectKeysList allQuotes [OffsetBy rindex, LimitTo 1]
      qu <- get $ head key
      case qu of
        Just q -> renderQuoteMessage q (fromSqlKey $ head key) m
        Nothing -> sendMessage m "Couldn't find any quotes!"
      return ()
  where
    allQuotes :: [Filter Quote]
    allQuotes = []

-- | @authorQuote@, which looks for a message of the form @!quote user u@,
-- selects a random quote from the database attributed to u and responds with that quote.
-- This is currently mis-documented in the help pages as its not quite working (the author matching is way to strict)
authorQ :: String -> Message -> DatabaseDiscord ()
authorQ t m = do
  num <- count userQuotes
  if num == 0
    then sendMessage m "Couldn't find any quotes matching that user!"
    else do
      rindex <- liftIO $ randomRIO (0, (num - 1))
      key <- selectKeysList userQuotes [OffsetBy rindex, LimitTo 1]
      qu <- get $ head key
      case qu of
        Just q -> renderQuoteMessage q (fromSqlKey $ head key) m
        Nothing -> sendMessage m "Couldn't find any quotes matching that user!"
      return ()
  where
    userQuotes :: [Filter Quote]
    userQuotes = [QuoteAuthor ==. t]

-- | @addQuote@, which looks for a message of the form
-- @!quote add "quoted text" - author@, and then stores said quote in the
-- database, returning the ID used.
addQ :: String -> String -> Message -> DatabaseDiscord ()
addQ qu author m = do
  now <- liftIO $ systemToUTCTime <$> getSystemTime
  let new = Quote qu author (Just $ fromIntegral $ messageId m) (Just $ fromIntegral $ messageChannel m) now
  added <- insert $ new
  let res = pack $ show $ fromSqlKey added
  renderCustomQuoteMessage ("Quote added as #" `append` res) new (fromSqlKey added) m

-- | @thisQuote@, which takes the replied message or the
-- previous message and stores said message as a quote in the database,
-- returning the ID used.
thisQ :: Message -> DatabaseDiscord ()
thisQ m = do
  q <- getReplyMessage m
  case q of
    (Just q') -> addMessageQuote q' m
    Nothing -> do
      q2 <- getPrecedingMessage m
      case q2 of
        (Just q') -> addMessageQuote q' m
        Nothing -> sendMessage m "Unable to add quote"

-- | @addMessageQuote@, adds a message as a quote to the database, checking that it passes the relevant tests
addMessageQuote :: Message -> Message -> DatabaseDiscord ()
addMessageQuote q' m = do
  num <- count [QuoteMsgId ==. Just (fromIntegral $ messageId q')]
  if num == 0
    then
      if not $ userIsBot (messageAuthor q')
        then do
          now <- liftIO $ systemToUTCTime <$> getSystemTime
          let new = Quote (unpack $ messageText q') (toMentionStr $ messageAuthor q') (Just $ fromIntegral $ messageId q') (Just $ fromIntegral $ messageChannel q') now
          added <- insert $ new
          let res = pack $ show $ fromSqlKey added
          renderCustomQuoteMessage ("Quote added as #" `append` res) new (fromSqlKey added) m
        else sendMessage m "Can't quote a bot"
    else sendMessage m "Message already quoted"

-- | @editQuote@, which looks for a message of the form
-- @!quote edit n "quoted text" - author@, and then updates quote with id n in the
-- database, to match the provided quote.
editQ :: Int64 -> String -> String -> Message -> DatabaseDiscord ()
editQ qId qu author m =
  requirePermission Any m $
    let k = toSqlKey qId
     in do
          oQu <- get k
          case oQu of
            Just (Quote _ _ _ _ _) -> do
              now <- liftIO $ systemToUTCTime <$> getSystemTime
              let new = Quote qu author (Just $ fromIntegral $ messageId m) (Just $ fromIntegral $ messageChannel m) now
              replace k $ new
              renderCustomQuoteMessage "Quote updated" new (fromIntegral $ messageId m) m
            Nothing -> sendMessage m "Couldn't update that quote!"

-- | @deleteQuote@, which looks for a message of the form @!quote delete n@,
-- and removes it from the database.
deleteQ :: Int64 -> Message -> DatabaseDiscord ()
deleteQ qId m =
  requirePermission Any m $
    let k = toSqlKey qId
     in do
          qu <- get k
          case qu of
            Just (Quote _ _ _ _ _) -> do
              delete k
              sendMessage m "Quote deleted"
            Nothing -> sendMessage m "Couldn't delete that quote!"

renderQuoteMessage :: Quote -> Int64 -> Message -> DatabaseDiscord ()
renderQuoteMessage = renderCustomQuoteMessage ""

renderCustomQuoteMessage :: Text -> Quote -> Int64 -> Message -> DatabaseDiscord ()
renderCustomQuoteMessage t (Quote txt author msgId cnlId dtm) qId m =
  void $ sendEmbedMessage m t (addColour Blue $ addTimestamp dtm $ addFooter (pack $ "Quote #" ++ show qId) $ simpleEmbed ((pack txt) <> "\n - " <> (pack author) <> maybeAddFooter link))
  where
    link = getMessageLink (messageGuild m) (fmap fromIntegral cnlId) (fmap fromIntegral msgId)
    maybeAddFooter l = if l == "" then "" else ("\n[source](" <> pack l <> ")")

showQuoteHelp :: HelpPage
showQuoteHelp = HelpPage "show" "show a quote by number" "**Show Quote**\nShows a quote by id\n\n*Usage:* `quote show <id>`" [] None

randomQuoteHelp :: HelpPage
randomQuoteHelp = HelpPage "random" "show a random quote" "**Random Quote**\nDisplays a random quote\n\n*Usage:* `quote random`" [] None

authorQuoteHelp :: HelpPage
authorQuoteHelp = HelpPage "user" "show a random quote by a user" "**Random User Quote**\nDisplays a random quote attributed to a particular user\n\n*Usage:* `quote user <author>`" [] Superuser

thisQuoteHelp :: HelpPage
thisQuoteHelp =
  HelpPage
    "this"
    "add another message as a quote"
    [r|**Quote This Message**
Adds an existing message as a quote. If the command is a reply, it uses the replied to message, otherwise it uses the immediatly preceding message.

*Usage:* `quote this`|]
    []
    None

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
    "edit"
    "edit a quote by number"
    [r|**Edit Quote**
Edit a quote by id
Requires moderation permission

*Usage:* `quote edit <id> "quote" - author`|]
    []
    Any

addQuoteHelp :: HelpPage
addQuoteHelp = HelpPage "add" "add a new quote" "**Add Quote**\nAdds a quote\n\n*Usage:* `quote add \"quote\" - author`" [] None

quoteHelp :: HelpPage
quoteHelp =
  HelpPage
    "quote"
    "store and retrieve quotes"
    [r|**Quotes**
Allows storing and retrieving quotes
Calling without arguments returns a random quote

*Usage:* `quote`|]
    [randomQuoteHelp, showQuoteHelp, authorQuoteHelp, addQuoteHelp, thisQuoteHelp, editQuoteHelp, deleteQuoteHelp]
    None

-- | @quotePlugin@ assembles the @quote@ command (consisting of @add@ and
-- @show@) and the database migration into a plugin.
quotePlugin :: Plugin
quotePlugin = plug {commands = [quote], onReactionAdds = [quoteReactionAdd], migrations = [quoteMigration], helpPages = [quoteHelp]}
