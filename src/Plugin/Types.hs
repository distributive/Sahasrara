module Plugin.Types where

import Data.Text (Text)
import Data.Attoparsec.Text
import Discord
import Discord.Types
import Database.Redis (Connection)
import Control.Monad.Trans.Reader

data FeatureEnv = FEnv { rconn :: Connection, discord :: DiscordHandle }
type FeatureFn = ReaderT FeatureEnv IO ()

-- This functionality comes in a few different flavours:
-- * Commands - your standard MessageCreate with some kind of parser applied (after prefix)
-- * MessageChange - called on MessageUpdate / MessageDelete / MessageDeleteBulk (as a map on MessageDelete)
-- * ReactionAdd - called on MessageReactionAdd
-- * ReactionDel - called on MessageReactionRemove / MessageReactionRemoveAll / MessageReactionRemoveEmoji 
-- (as maps on MessageReactionRemove)
-- * Other - events not covered here (should fire rarely, so not too much of a worry)
-- * CronJob - runs on a given timeframe (represented as a delay in microseconds)
data Feature =
    Command { name :: Text, parser :: Parser (Message -> FeatureFn) }
    | MessageChange {
        -- Bool represents whether the message was updated or deleted.
        onMessageChange :: Bool -> ChannelId -> MessageId -> FeatureFn }
    | ReactionAdd { onReactionAdd :: ReactionInfo -> FeatureFn }
    | ReactionDel { onReactionDelete :: ReactionInfo -> FeatureFn }
    | Other { onOtherEvent :: Event -> FeatureFn }
    | CronJob { timeframe :: Int, onCron :: FeatureFn }

type Plugin = [Feature]

