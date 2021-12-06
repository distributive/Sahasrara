{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
import Database.Persist.Sqlite (Filter, SelectOpt (LimitTo, OffsetBy), (==.))
import Database.Persist.TH
import Discord.Types
import GHC.Int (Int64)
import System.Random (randomRIO)
import Tablebot.Plugin
import Tablebot.Plugin.Database
import Tablebot.Plugin.Discord
  ( findGuild,
    getMessage,
    getMessageLink,
    getPrecedingMessage,
    getReplyMessage,
    sendEmbedMessage,
    sendMessage,
    toMention,
    toMention',
  )
import Tablebot.Plugin.Embed
import Tablebot.Plugin.Exception (BotException (GenericException), catchBot, throwBot)
import Tablebot.Plugin.Permission (requirePermission)
import Tablebot.Plugin.SmartCommand
import Text.RawString.QQ (r)

-- Our Quote table in the database. This is fairly standard for Persistent,
-- however you should note the name of the migration made.
share
  [mkPersist sqlSettings, mkMigrate "quoteMigration"]
  [persistLowerCase|
Quote
    quote Text
    author Text
    submitter Text
    msgId Int
    cnlId Int
    time UTCTime
    deriving Show
|]

-- | @quoteReactionAdd@ creates an event for when a reaction is added to a message.
-- If the reaction is :speech_balloon:, the message is added as a quote
quoteReactionAdd :: ReactionAdd
quoteReactionAdd = ReactionAdd quoteReaction
  where
    quoteReaction ri
      | emojiName (reactionEmoji ri) == "\x1F4AC" = do
        m <- getMessage (reactionChannelId ri) (reactionMessageId ri)
        case m of
          Left _ -> pure ()
          Right mes -> addMessageQuote (reactionUserId ri) mes mes
      | otherwise = return ()

-- | Our quote command, which combines various functions to create, display and update quotes.
quote :: Command
quote =
  Command
    "quote"
    (parseComm quoteComm)
    [addQuote, editQuote, thisQuote, authorQuote, showQuote, deleteQuote, randomQuote]
  where
    quoteComm ::
      WithError
        "Unknown quote functionality."
        (Either () (Either Int64 (RestOfInput Text))) ->
      Message ->
      DatabaseDiscord ()
    quoteComm (WErr (Left ())) = randomQ
    quoteComm (WErr (Right (Left t))) = showQ t
    quoteComm (WErr (Right (Right (ROI t)))) = authorQ t

addQuote :: Command
addQuote = Command "add" (parseComm addComm) []
  where
    addComm ::
      WithError "Quote format incorrect!\nFormat is: .quote \"quote\" - author" (Quoted Text, Exactly "-", RestOfInput Text) ->
      Message ->
      DatabaseDiscord ()
    addComm (WErr (Qu qu, _, ROI author)) = addQ qu author

editQuote :: Command
editQuote = Command "edit" (parseComm editComm) []
  where
    editComm ::
      WithError
        "Edit format incorrect!\nFormat is: .quote edit quoteId \"new quote\" - author"
        (Int64, Quoted Text, Exactly "-", RestOfInput Text) ->
      Message ->
      DatabaseDiscord ()
    editComm (WErr (qId, Qu qu, _, ROI author)) = editQ qId qu author

thisQuote :: Command
thisQuote = Command "this" (parseComm thisComm) []
  where
    thisComm :: Message -> DatabaseDiscord ()
    thisComm = thisQ

authorQuote :: Command
authorQuote = Command "author" (parseComm authorComm) []
  where
    authorComm ::
      WithError "Quote format incorrect!\nExpected author name to find quotes for after .quote author" (RestOfInput Text) ->
      Message ->
      DatabaseDiscord ()
    authorComm (WErr (ROI author)) = authorQ author

showQuote :: Command
showQuote = Command "show" (parseComm showComm) []
  where
    showComm ::
      WithError "Quote format incorrect!\nExpected quote number to show, e.g. .quote show 420" Int64 ->
      Message ->
      DatabaseDiscord ()
    showComm (WErr qId) = showQ qId

deleteQuote :: Command
deleteQuote = Command "delete" (parseComm deleteComm) []
  where
    deleteComm ::
      WithError "Quote format incorrect!\nExpected quote number to delete, e.g. .quote delete 420" Int64 ->
      Message ->
      DatabaseDiscord ()
    deleteComm (WErr qId) = deleteQ qId

randomQuote :: Command
randomQuote = Command "random" (parseComm randomComm) []
  where
    randomComm :: Message -> DatabaseDiscord ()
    randomComm = randomQ

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
randomQ = filteredRandomQuote [] "Couldn't find any quotes!"

-- | @authorQuote@, which looks for a message of the form @!quote author u@,
-- selects a random quote from the database attributed to u and responds with that quote.
authorQ :: Text -> Message -> DatabaseDiscord ()
authorQ t = filteredRandomQuote [QuoteAuthor ==. t] "Couldn't find any quotes with that author!"

-- authorQ t m = fromMaybe (filteredRandomQuote (getFilter t) errText m) (processUid <$> uid)
--   where
--     errText = "Couldn't find any quotes with that author!"
--     uid = fromMention t
--     catchBot' id' (GenericException "quote exception" _) = filteredRandomQuote (getFilter $ "<@" <> (pack $ show id') <> ">") errText m
--     catchBot' _ e = throwBot e
--     processUid id' = catchBot (filteredRandomQuote' (getFilter $ toMention' id') errText m) (catchBot' id')
--     getFilter t' = [QuoteAuthor ==. t']

-- | @filteredRandomQuote@ selects a random quote that meets a
-- given criteria, and returns that as the response, sending the user a message if the
-- quote cannot be found.
filteredRandomQuote :: [Filter Quote] -> Text -> Message -> DatabaseDiscord ()
filteredRandomQuote quoteFilter errorMessage m = catchBot (filteredRandomQuote' quoteFilter errorMessage m) catchBot'
  where
    catchBot' (GenericException "quote exception" _) = sendMessage m errorMessage
    catchBot' e = throwBot e

-- | @filteredRandomQuote'@ selects a random quote that meets a
-- given criteria, and returns that as the response, throwing an exception if something
-- goes wrong.
filteredRandomQuote' :: [Filter Quote] -> Text -> Message -> DatabaseDiscord ()
filteredRandomQuote' quoteFilter errorMessage m = do
  num <- count quoteFilter
  if num == 0
    then throwBot (GenericException "quote exception" (unpack errorMessage))
    else do
      rindex <- liftIO $ randomRIO (0, num - 1)
      key <- selectKeysList quoteFilter [OffsetBy rindex, LimitTo 1]
      qu <- get $ head key
      case qu of
        Just q -> renderQuoteMessage q (fromSqlKey $ head key) m
        Nothing -> throwBot (GenericException "quote exception" (unpack errorMessage))

-- | @addQuote@, which looks for a message of the form
-- @!quote add "quoted text" - author@, and then stores said quote in the
-- database, returning the ID used.
addQ :: Text -> Text -> Message -> DatabaseDiscord ()
addQ qu author m = do
  now <- liftIO $ systemToUTCTime <$> getSystemTime
  let new = Quote qu author (toMention $ messageAuthor m) (fromIntegral $ messageId m) (fromIntegral $ messageChannel m) now
  added <- insert new
  let res = pack $ show $ fromSqlKey added
  renderCustomQuoteMessage ("Quote added as #" `append` res) new (fromSqlKey added) m

-- | @thisQuote@, which takes the replied message or the
-- previous message and stores said message as a quote in the database,
-- returning the ID used.
thisQ :: Message -> DatabaseDiscord ()
thisQ m = do
  q <- getReplyMessage m
  case q of
    (Just q') -> addMessageQuote (userId $ messageAuthor m) q' m
    Nothing -> do
      q2 <- getPrecedingMessage m
      case q2 of
        (Just q') -> addMessageQuote (userId $ messageAuthor m) q' m
        Nothing -> sendMessage m "Unable to add quote"

-- | @addMessageQuote@, adds a message as a quote to the database, checking that it passes the relevant tests
addMessageQuote :: UserId -> Message -> Message -> DatabaseDiscord ()
addMessageQuote submitter q' m = do
  num <- count [QuoteMsgId ==. fromIntegral (messageId q')]
  if num == 0
    then
      if not $ userIsBot (messageAuthor q')
        then do
          now <- liftIO $ systemToUTCTime <$> getSystemTime
          let new =
                Quote
                  (messageText q')
                  (toMention $ messageAuthor q')
                  (toMention' submitter)
                  (fromIntegral $ messageId q')
                  (fromIntegral $ messageChannel q')
                  now
          added <- insert new
          let res = pack $ show $ fromSqlKey added
          renderCustomQuoteMessage ("Quote added as #" `append` res) new (fromSqlKey added) m
        else sendMessage m "Can't quote a bot"
    else sendMessage m "Message already quoted"

-- | @editQuote@, which looks for a message of the form
-- @!quote edit n "quoted text" - author@, and then updates quote with id n in the
-- database, to match the provided quote.
editQ :: Int64 -> Text -> Text -> Message -> DatabaseDiscord ()
editQ qId qu author m =
  requirePermission Any m $
    let k = toSqlKey qId
     in do
          oQu <- get k
          case oQu of
            Just Quote {} -> do
              now <- liftIO $ systemToUTCTime <$> getSystemTime
              let new = Quote qu author (toMention $ messageAuthor m) (fromIntegral $ messageId m) (fromIntegral $ messageChannel m) now
              replace k new
              renderCustomQuoteMessage "Quote updated" new qId m
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
            Just Quote {} -> do
              delete k
              sendMessage m "Quote deleted"
            Nothing -> sendMessage m "Couldn't delete that quote!"

renderQuoteMessage :: Quote -> Int64 -> Message -> DatabaseDiscord ()
renderQuoteMessage = renderCustomQuoteMessage ""

renderCustomQuoteMessage :: Text -> Quote -> Int64 -> Message -> DatabaseDiscord ()
renderCustomQuoteMessage t (Quote txt author submitter msgId cnlId dtm) qId m = do
  guild <- findGuild m
  let link = getLink guild
  void $
    sendEmbedMessage
      m
      t
      ( addColour Blue $
          addTimestamp dtm $
            addFooter (pack $ "Quote #" ++ show qId) $
              simpleEmbed (txt <> "\n - " <> author <> maybeAddFooter link)
      )
  where
    getLink :: Maybe GuildId -> Maybe Text
    getLink = fmap (\x -> getMessageLink x (fromIntegral cnlId) (fromIntegral msgId))
    maybeAddFooter :: Maybe Text -> Text
    maybeAddFooter (Just l) = "\n[source](" <> l <> ") - added by " <> submitter
    maybeAddFooter Nothing = ""

showQuoteHelp :: HelpPage
showQuoteHelp =
  HelpPage
    "show"
    "show a quote by number"
    "**Show Quote**\nShows a quote by id\n\n*Usage:* `quote show <id>`"
    []
    None

randomQuoteHelp :: HelpPage
randomQuoteHelp =
  HelpPage
    "random"
    "show a random quote"
    "**Random Quote**\nDisplays a random quote\n\n*Usage:* `quote random`"
    []
    None

authorQuoteHelp :: HelpPage
authorQuoteHelp =
  HelpPage
    "user"
    "show a random quote by a user"
    "**Random User Quote**\nDisplays a random quote attributed to a particular user\n\n*Usage:* `quote user <author>`"
    []
    Superuser

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
Allows storing and retrieving quotes.
Calling without arguments returns a random quote. Calling with a number returns that quote number. Calling with a mention or name gives a random quote by that person.

*Usage:* `quote` or `q`|]
    [randomQuoteHelp, showQuoteHelp, authorQuoteHelp, addQuoteHelp, thisQuoteHelp, editQuoteHelp, deleteQuoteHelp]
    None

-- | @quotePlugin@ assembles the @quote@ command (consisting of @add@ and
-- @show@) and the database migration into a plugin.
quotePlugin :: Plugin
quotePlugin =
  (plug "quote")
    { commands = [quote, commandAlias "q" quote],
      onReactionAdds = [quoteReactionAdd],
      migrations = [quoteMigration],
      helpPages = [quoteHelp]
    }
