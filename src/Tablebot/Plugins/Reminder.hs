{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Tablebot.Plugins.Quote
-- Description : A complex example using databases and cron jobs.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This is an example plugin which allows user to ask the bot to remind them about
-- something later in time.
module Tablebot.Plugins.Reminder
  ( reminderPlugin,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (append, pack)
import Data.Time.Calendar.MonthDay
  ( monthAndDayToDayOfYear,
    monthLength,
  )
import Data.Time.Calendar.OrdinalDate
  ( fromOrdinalDate,
    isLeapYear,
  )
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Word (Word64)
import Database.Esqueleto
import Database.Persist qualified as P (delete)
import Database.Persist.TH
import Discord.Types
import Tablebot.Plugin
import Tablebot.Plugin.Discord
  ( Message,
    getMessage,
    sendMessageVoid,
  )
import Tablebot.Plugin.Parser (number, quoted, space)
import Text.Megaparsec
import Text.RawString.QQ

-- Our Reminder table in the database. This is fairly standard for Persistent,
-- however you should note the name of the migration made.
share
  [mkPersist sqlSettings, mkMigrate "reminderMigration"]
  [persistLowerCase|
Reminder
    reminderCid Word64
    reminderMid Word64
    time UTCTime
    content String
    deriving Show
|]

-- | @dateTimeParser@ parses a string of the form DD/MM/YYYY HH:MM into a
-- 'UTCTime'.
dateTimeParser :: Parser UTCTime
-- TODO: better parsing.
dateTimeParser = do
  day <- number
  single '/'
  month <- number
  when (month > 12) $ fail "There are only twelve months in a year!"
  single '/'
  year <- number
  let leapYear = isLeapYear (toInteger year)
  when (day > monthLength leapYear month) $ fail "That month doesn't have enough days!"
  space
  hour <- number
  single ':'
  minute <- number
  when (hour >= 24) $ fail "There are only 24 hours in a day!"
  when (minute >= 60) $ fail "There are only sixty minutes in an hour!"
  let monthday = monthAndDayToDayOfYear leapYear month day
  let yearday = fromOrdinalDate (toInteger year) monthday
  let difftime = secondsToDiffTime $ toInteger $ (minute * 60) + (hour * 60 * 60)
  return $ UTCTime yearday difftime

-- @reminderParser@ parses a reminder request of the form
-- @!remind "reminder" at time@.
reminderParser :: Parser (Message -> DatabaseDiscord ())
reminderParser = do
  content <- quoted
  chunk " at "
  time <- dateTimeParser
  return $ addReminder time content

-- @addReminder@ takes a @time@ to remind at and the @content@ of a reminder
-- and adds a reminder at that time. Note that this is all done in UTC, so
-- currently ignores the user's timezone... (TODO fix)
addReminder :: UTCTime -> String -> Message -> DatabaseDiscord ()
addReminder time content m = do
  let (Snowflake cid) = messageChannel m
      (Snowflake mid) = messageId m
  added <- insert $ Reminder cid mid time content
  let res = pack $ show $ fromSqlKey added
  sendMessageVoid m ("Reminder added as #" `append` res)

-- | @reminderCommand@ is a command implementing the functionality in
-- @reminderParser@ and @addReminder@.
reminderCommand :: Command
reminderCommand = Command "remind" reminderParser

-- | @reminderCron@ is a cron job that checks every minute to see if a reminder
-- has passed, and if so sends a message using the stored information about the
-- message originally triggering it in the database.
reminderCron :: DatabaseDiscord ()
reminderCron = do
  now <- liftIO $ systemToUTCTime <$> getSystemTime
  liftIO $ putStrLn $ "running reminder cron at " ++ show now
  entitydue <- select $
    from $ \r -> do
      where_ (r ^. ReminderTime <=. val now)
      return r
  liftIO $ mapM_ (print . entityVal) entitydue
  forM_ entitydue $ \r ->
    let (Reminder cid mid time content) = entityVal r
     in do
          res <- getMessage (Snowflake cid) (Snowflake mid)
          case res of
            Left e -> pure ()
            Right mess -> do
              let (Snowflake uid) = userId (messageAuthor mess)
              sendMessageVoid mess $
                pack $
                  "Reminder to <@" ++ show uid ++ ">! " ++ content
              P.delete (entityKey r)

reminderHelp :: HelpPage
reminderHelp =
  HelpPage
    "remind"
    "ask the bot to remind you to do things in the future"
    [r|**Reminders**
Send a reminder to yourself or others. Pick a date and time, and the tablebot will poke you to remember at your preordained moment.

*Usage:* `remind "reminder" at <time>`|]
    []

-- | @reminderPlugin@ builds a plugin providing reminder asking functionality
-- (@reminderCommand@), reminding functionality (via the cron job specified by
-- @reminderCron@) and the database information.
reminderPlugin :: Plugin
reminderPlugin =
  plug
    { commands = [reminderCommand],
      cronJobs = [CronJob 60000000 reminderCron],
      migrations = [reminderMigration],
      helpPages = [reminderHelp]
    }
