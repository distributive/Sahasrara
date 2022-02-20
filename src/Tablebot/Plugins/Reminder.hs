{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : Tablebot.Plugins.Quote
-- Description : A complex example using databases and cron jobs.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which allows user to ask the bot to remind them about
-- something later in time.
module Tablebot.Plugins.Reminder
  ( reminderPlugin,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict (singleton)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.Time.LocalTime.TimeZone.Olson.Parse (getTimeZoneSeriesFromOlsonFile)
import Data.Word (Word64)
import Database.Esqueleto hiding (delete, insert)
import Database.Persist.TH
import Discord.Types
import Duckling.Core (Dimension (Time), Entity (value), Lang (EN), Region (GB), ResolvedVal (RVal), Seal (Seal), currentReftime, makeLocale, parse)
import Duckling.Resolve (Context (..), DucklingTime, Options (..))
import Duckling.Time.Types (InstantValue (InstantValue), SingleTimeValue (SimpleValue), TimeValue (TimeValue))
import Tablebot.Utility
import Tablebot.Utility.Database
import Tablebot.Utility.Discord (getMessage, sendChannelMessage, sendCustomReplyMessage, sendMessage, toTimestamp)
import Tablebot.Utility.Permission (requirePermission)
import Tablebot.Utility.SmartParser (PComm (parseComm), Quoted (Qu), RestOfInput (ROI), WithError (..))
import Text.RawString.QQ (r)

-- Our Reminder table in the database. This is fairly standard for Persistent,
-- however you should note the name of the migration made.
share
  [mkPersist sqlSettings, mkMigrate "reminderMigration"]
  [persistLowerCase|
Reminder
    reminderCid Word64
    reminderMid Word64
    user Word64
    time UTCTime
    content String
    deriving Show
|]

-- Below duckling code was greatly helped by dixonary's example project, below
-- <https://github.com/dixonary/duckling-example>

-- | @ducklingDateTime@ might return the UTCTime represented by the given text when the current time
-- is given as a DucklingTime. If it cannot parse the Text, Nothing is given.
ducklingDateTime :: DucklingTime -> Text -> Maybe UTCTime
ducklingDateTime now rawString = do
  entity <- listToMaybe (Duckling.Core.parse rawString context options [Seal Time])
  zt <- getVal entity
  return $ zonedTimeToUTC zt
  where
    context = Context {referenceTime = now, locale = makeLocale EN (Just GB)}
    options = Options {withLatent = True}
    getVal :: Duckling.Core.Entity -> Maybe ZonedTime
    getVal pr = case value pr of
      RVal Time (TimeValue (SimpleValue (InstantValue x _)) _ _) -> Just x
      _ -> Nothing

-- @reminderParser@ parses a reminder request of the form
-- @!remind "reminder" <format>@, where format is a format that Duckling can parse.
-- TODO: get timezone info and such ahead of time
reminderParser ::
  WithError "Incorrect reminder format!\nReminder needs a reminder (in quotes) followed by the time to be reminded at." (Quoted String, RestOfInput Text) ->
  Message ->
  DatabaseDiscord ()
reminderParser (WErr (Qu content, ROI rawString)) m = do
  let tz = "Europe/London" :: Text
  tzs <- liftIO $ getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" <> unpack tz
  now <- liftIO $ currentReftime (singleton tz tzs) tz
  let mTime = ducklingDateTime now rawString
  time <- case mTime of
    Just a -> return a
    Nothing -> sendMessage m failText >> fail (unpack failText)
  addReminder time content m
  where
    failText = "Date could not be parsed: `" <> rawString <> "`"

-- @addReminder@ takes a @time@ to remind at and the @content@ of a reminder
-- and adds a reminder at that time. Note that this is all done in UTC, so
-- currently ignores the user's timezone... (TODO fix)
addReminder :: UTCTime -> String -> Message -> DatabaseDiscord ()
addReminder time content m = do
  let (Snowflake cid) = messageChannel m
      (Snowflake mid) = messageId m
      (Snowflake uid) = userId $ messageAuthor m
  added <- insert $ Reminder cid mid uid time content
  let res = pack $ show $ fromSqlKey added
  sendMessage m ("Reminder " <> res <> " set for " <> toTimestamp time <> " with message `" <> pack content <> "`")

-- @deleteReminder@ takes a reminder Id and deletes it from the list of awating reminders.
deleteReminder :: WithError "Missing required argument" (Int) -> Message -> DatabaseDiscord ()
deleteReminder (WErr rid) m = requirePermission Any m $ do
  delete k
  sendMessage m ("Reminder " <> pack (show rid) <> " deleted.")
  where
    k :: Key Reminder
    k = toSqlKey $ fromIntegral rid

-- | @reminderCommand@ is a command implementing the functionality in
-- @reminderParser@ and @addReminder@.
reminderCommand :: Command
reminderCommand = Command "remind" (parseComm reminderParser) [deleteReminderCommand]

-- | @deleteReminderCommand@ is a subcommand that provides the deletion feature.
deleteReminderCommand :: Command
deleteReminderCommand = Command "delete" (parseComm deleteReminder) []

-- | @reminderCron@ is a cron job that checks every minute to see if a reminder
-- has passed, and if so sends a message using the stored information about the
-- message originally triggering it in the database.
reminderCron :: DatabaseDiscord ()
reminderCron = do
  now <- liftIO $ systemToUTCTime <$> getSystemTime
  liftIO $ debugPrint $ "running reminder cron at " ++ show now
  entitydue <- liftSql $
    select $
      from $ \re -> do
        where_ (re ^. ReminderTime <=. val now)
        return re
  forM_ entitydue $ \re ->
    let (Reminder cid mid uid _time content) = entityVal re
     in do
          liftIO . print $ entityVal re
          res <- getMessage (Snowflake cid) (Snowflake mid)
          case res of
            Left _ -> do
              sendChannelMessage (fromIntegral cid) (pack $ "Reminder to <@" ++ show uid ++ ">! " ++ content)
              delete (entityKey re)
            Right mess -> do
              sendCustomReplyMessage mess (Snowflake mid) True $
                pack $
                  "Reminder to <@" ++ show uid ++ ">! " ++ content
              delete (entityKey re)

reminderHelp :: HelpPage
reminderHelp =
  HelpPage
    "remind"
    []
    "ask the bot to remind you to do things in the future"
    [r|**Reminders**
Send a reminder to yourself or others. Pick a date and time, and the tablebot will poke you to remember at your preordained moment.

Uses duckling (<https://github.com/facebook/duckling>) to parse time and dates, and thus is quite flexible, if you want to use natural language for example.

*Usage:* `remind "reminder" <at|in|on> <time or duration>`|]
    [deleteReminderHelp]
    None

deleteReminderHelp :: HelpPage
deleteReminderHelp =
  HelpPage
    "delete"
    []
    "delete a reminder by number"
    [r|**Delete Reminder**
Delete a reminder by id
Requires moderation permission

*Usage:* `remind delete <id>`|]
    []
    Any

-- | @reminderPlugin@ builds a plugin providing reminder asking functionality
-- (@reminderCommand@), reminding functionality (via the cron job specified by
-- @reminderCron@) and the database information.
reminderPlugin :: Plugin
reminderPlugin =
  (plug "reminder")
    { commands = [reminderCommand],
      cronJobs = [CronJob 60000000 reminderCron],
      migrations = [reminderMigration],
      helpPages = [reminderHelp]
    }
