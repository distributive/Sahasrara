module Tablebot.Plugin.Types where

import Data.Text (Text)
import Text.Parsec.Text (Parser)
import Discord (DiscordHandler)
import Discord.Types
    (Event, Message, ChannelId, MessageId, ReactionInfo)
import Database.Selda

type SeldaDiscord b a = SeldaT b DiscordHandler a
type SD a = forall b. SeldaDiscord b a

-- Bot functionality comes in a few different flavours.
-- * Commands - your standard MessageCreate with some kind of parser applied (after prefix)
data Command b = Command { name :: Text, commandParser :: Parser (Message -> SeldaDiscord b ()) }
-- * InlineCommand - for commands called with fancy brackets and similar
newtype InlineCommand b = InlineCommand { inlineCommandParser :: Parser (Message -> SeldaDiscord b ()) }
-- * MessageChange - called on MessageUpdate / MessageDelete / MessageDeleteBulk (as a map on MessageDelete)
newtype MessageChange b = MessageChange {
        -- Bool represents whether the message was updated or deleted.
        onMessageChange :: Bool -> ChannelId -> MessageId -> SeldaDiscord b () }
-- * ReactionAdd - called on MessageReactionAdd
newtype ReactionAdd b = ReactionAdd { onReactionAdd :: ReactionInfo -> SeldaDiscord b () }
-- * ReactionDel - called on MessageReactionRemove / MessageReactionRemoveAll / MessageReactionRemoveEmoji 
-- (as maps on MessageReactionRemove)
newtype ReactionDel b = ReactionDel { onReactionDelete :: ReactionInfo -> SeldaDiscord b () }
-- * Other - events not covered here (should fire rarely, so not too much of a worry)
newtype Other b = Other { onOtherEvent :: Event -> SeldaDiscord b () }
-- * CronJob - runs on a given timeframe (represented as a delay in microseconds)
data CronJob b = CronJob { timeframe :: Int, onCron :: SeldaDiscord b () }

data Plugin b = Pl {
    commands :: [Command b],
    inlineCommands :: [InlineCommand b],
    onMessageChanges :: [MessageChange b],
    onReactionAdds :: [ReactionAdd b],
    onReactionDeletes :: [ReactionDel b],
    otherEvents :: [Other b],
    cronJobs :: [CronJob b]
}

plug :: Plugin b
plug = Pl [] [] [] [] [] [] []

combinePlugins :: [Plugin b] -> Plugin b
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