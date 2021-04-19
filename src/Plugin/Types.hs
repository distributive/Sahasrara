module Plugin.Types where

import Data.Text (Text)
import Data.Attoparsec.Text
import Discord
import Discord.Types
import Database.Selda

type FH b = SeldaT b DiscordHandler ()

-- This functionality comes in a few different flavours:
-- * Commands - your standard MessageCreate with some kind of parser applied (after prefix)
-- * InlineCommand - for commands called with fancy brackets and similar
-- * MessageChange - called on MessageUpdate / MessageDelete / MessageDeleteBulk (as a map on MessageDelete)
-- * ReactionAdd - called on MessageReactionAdd
-- * ReactionDel - called on MessageReactionRemove / MessageReactionRemoveAll / MessageReactionRemoveEmoji 
-- (as maps on MessageReactionRemove)
-- * Other - events not covered here (should fire rarely, so not too much of a worry)
-- * CronJob - runs on a given timeframe (represented as a delay in microseconds)
data Feature b =
    Command { name :: Text, commandParser :: Parser (Message -> FH b) }
    | InlineCommand { commandParser :: Parser (Message -> FH b) }
    | MessageChange {
        -- Bool represents whether the message was updated or deleted.
        onMessageChange :: Bool -> ChannelId -> MessageId -> FH b }
    | ReactionAdd { onReactionAdd :: ReactionInfo -> FH b }
    | ReactionDel { onReactionDelete :: ReactionInfo -> FH b }
    | Other { onOtherEvent :: Event -> FH b }
    | CronJob { timeframe :: Int, onCron :: FH b }

type Plugin b = [Feature b]

