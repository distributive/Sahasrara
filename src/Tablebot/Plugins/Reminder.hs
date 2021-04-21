module Tablebot.Plugins.Reminder (
    reminderPlugin
) where

import Tablebot.Plugin.Parser
import Tablebot.Plugin.Types
import Tablebot.Plugin.Discord

import Discord.Types
import Data.Word (Word64)
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.MonthDay
import Text.Parsec
import Database.Esqueleto
import Database.Persist qualified as P (delete)
import Database.Persist.TH
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (pack, append)

share [mkPersist sqlSettings, mkMigrate "reminderMigration"] [persistLowerCase|
Reminder
    reminderCid Word64
    reminderMid Word64
    time UTCTime
    content String
    deriving Show
|]

-- Parses DD/MM/YYYY HH:MM
-- TODO: better parsing.
dateTimeParser :: Parser UTCTime
dateTimeParser = do
    day <- number
    char '/'
    month <- number
    when (month > 12) $ fail "There are only twelve months in a year!"
    char '/'
    year <- number
    let leapYear = isLeapYear (toInteger year)
    when (day > monthLength leapYear month) $ fail "That month doesn't have enough days!"
    space
    hour <- number
    char ':'
    minute <- number
    when (hour >= 24) $ fail "There are only 24 hours in a day!"
    when (minute >= 60) $ fail "There are only sixty minutes in an hour!"
    let monthday = monthAndDayToDayOfYear leapYear month day
    let yearday = fromOrdinalDate (toInteger year) monthday
    let difftime = secondsToDiffTime $ toInteger $ (minute * 60) + (hour * 60 * 60)
    return $ UTCTime yearday difftime

-- !remind "reminder" at (time)
reminderParser :: Parser (Message -> DatabaseDiscord ())
reminderParser = do
    content <- quoted
    string " at "
    time <- dateTimeParser
    return $ addReminder time content

-- TODO: timezones...
addReminder :: UTCTime -> String -> Message -> DatabaseDiscord ()
addReminder time content m = do
    let (Snowflake cid) = messageChannel m
        (Snowflake mid) = messageId m
    added <- insert $ Reminder cid mid time content
    let res = pack $ show $ fromSqlKey added
    sendMessageVoid m ("Reminder added as #" `append` res)

reminderCommand :: Command
reminderCommand = Command "remind" reminderParser

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
                    sendMessageVoid mess $ pack $
                        "Reminder to <@" ++ show uid ++ ">! " ++ content
                    P.delete (entityKey r)

reminderPlugin :: Plugin
reminderPlugin = plug { commands = [reminderCommand], cronJobs = [CronJob 60000000 reminderCron], migrations = [reminderMigration] }