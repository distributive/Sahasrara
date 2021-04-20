module Tablebot.Plugin.Types where

import Data.Text (Text)
import Text.Parsec.Text (Parser)
import Discord (DiscordHandler)
import Discord.Types
    (Event, Message, ChannelId, MessageId, ReactionInfo)
import Database.Persist.Sqlite (SqlPersistT)

type DatabaseDiscord a = SqlPersistT DiscordHandler a

-- Bot functionality comes in a few different flavours.
-- * Commands - your standard MessageCreate with some kind of parser applied (after prefix)
data Command = Command { name :: Text, commandParser :: Parser (Message -> DatabaseDiscord ()) }
-- * InlineCommand - for commands called with fancy brackets and similar
newtype InlineCommand = InlineCommand { inlineCommandParser :: Parser (Message -> DatabaseDiscord ()) }
-- * MessageChange - called on MessageUpdate / MessageDelete / MessageDeleteBulk (as a map on MessageDelete)
newtype MessageChange = MessageChange {
        -- Bool represents whether the message was updated or deleted.
        onMessageChange :: Bool -> ChannelId -> MessageId -> DatabaseDiscord () }
-- * ReactionAdd - called on MessageReactionAdd
newtype ReactionAdd = ReactionAdd { onReactionAdd :: ReactionInfo -> DatabaseDiscord () }
-- * ReactionDel - called on MessageReactionRemove / MessageReactionRemoveAll / MessageReactionRemoveEmoji 
-- (as maps on MessageReactionRemove)
newtype ReactionDel = ReactionDel { onReactionDelete :: ReactionInfo -> DatabaseDiscord () }
-- * Other - events not covered here (should fire rarely, so not too much of a worry)
newtype Other = Other { onOtherEvent :: Event -> DatabaseDiscord () }
-- * CronJob - runs on a given timeframe (represented as a delay in microseconds)
data CronJob = CronJob { timeframe :: Int, onCron :: DatabaseDiscord () }

data Plugin = Pl {
    commands :: [Command],
    inlineCommands :: [InlineCommand],
    onMessageChanges :: [MessageChange],
    onReactionAdds :: [ReactionAdd],
    onReactionDeletes :: [ReactionDel],
    otherEvents :: [Other],
    cronJobs :: [CronJob]
}

plug :: Plugin
plug = Pl [] [] [] [] [] [] []

combinePlugins :: [Plugin] -> Plugin
combinePlugins [] = plug
combinePlugins (p : ps) = let p' = combinePlugins ps
    in Pl {
        commands = merge commands p p',
        inlineCommands = merge inlineCommands p p',
        onMessageChanges = merge onMessageChanges p p',
        onReactionAdds = merge onReactionAdds p p',
        onReactionDeletes = merge onReactionDeletes p p',
        otherEvents = merge otherEvents p p',
        cronJobs = merge cronJobs p p'
    }
    where merge f p p' = f p +++ f p'
          -- We expect empty list to be very common in this process, so we add
          -- the special case where the second element is empty. This is
          -- because plugins are unlikely to define every possible kind of
          -- event, so we will get many [] instances.
          (+++) :: [a] -> [a] -> [a]
          [] +++ ys = ys
          xs +++ [] = xs
          (x:xs) +++ ys = x : xs +++ ys