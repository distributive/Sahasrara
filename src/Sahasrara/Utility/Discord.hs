-- |
-- Module      : Sahasrara.Utility.Discord
-- Description : Discord helpers for building plugins.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains helpful Discord functionality for building plugins
-- without having to lift Discord operations constantly.
module Sahasrara.Utility.Discord
  ( sendMessage,
    sendCustomMessage,
    sendChannelMessage,
    sendReplyMessage,
    sendCustomReplyMessage,
    sendEmbedMessage,
    sendChannelEmbedMessage,
    sendEmbedInteraction,
    sendEmbedInteractionWithButtons,
    basicButton,
    reactToMessage,
    findGuild,
    listGuildMembers,
    findEmoji,
    getChannel,
    getMessage,
    getMessageMember,
    getReplyMessage,
    getPrecedingMessage,
    toMention,
    toMention',
    fromMention,
    fromMentionStr,
    toTimestamp,
    toTimestamp',
    formatEmoji,
    formatFromEmojiName,
    toRelativeTime,
    getMessageLink,
    Message,
    Format (..),
    formatText,
    formatInput,
    TimeFormat,
    extractFromSnowflake,
    createApplicationCommand,
    removeApplicationCommandsNotInList,
    interactionResponseDefer,
    interactionResponseDeferUpdateMessage,
    interactionResponseMessage,
    interactionResponseCustomMessage,
    interactionResponseComponentsUpdateMessage,
    interactionResponseAutocomplete,
  )
where

import Control.Monad.Cont (liftIO)
import Control.Monad.Exception (MonadException (throw))
import Data.Char (isDigit)
import Data.Default (Default (def))
import Data.Foldable (msum)
import Data.List ((\\))
import Data.Map.Strict (keys)
import Data.Maybe (listToMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Discord (Cache (cacheGuilds), DiscordHandler, RestCallErrorCode, readCache, restCall)
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import GHC.Word (Word64)
import Sahasrara.Internal.Cache (fillEmojiCache, lookupEmojiCache)
import Sahasrara.Internal.Embed (Embeddable (..))
import Sahasrara.Utility (EnvDatabaseDiscord, MessageDetails, convertMessageFormatBasic, convertMessageFormatInteraction, liftDiscord, messageDetailsBasic, messageDetailsEmbeds, messageDetailsComponents)
import Sahasrara.Utility.Exception (BotException (..))
import System.Environment (lookupEnv)

-- | @sendMessage@ sends the input message @t@ in the same channel as message
-- @m@.
sendMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s ()
sendMessage m t = do
  res <- liftDiscord . restCall $ R.CreateMessage (messageChannelId m) t
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendCustomMessage@ sends the input message @mdo@ in the same channel as
-- message @m@.
--
-- As opposed to @sendMessage@, this function takes in a MessageDetails, to
-- allow full functionality. Unless you are dealing with components or some
-- other specific message data, you shouldn't use this function.
sendCustomMessage ::
  Message ->
  MessageDetails ->
  EnvDatabaseDiscord s ()
sendCustomMessage m t = do
  res <- liftDiscord . restCall $ R.CreateMessageDetailed (messageChannelId m) (convertMessageFormatBasic t)
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendChannelMessage@ sends the input message @t@ into the provided channel
-- @m@.
sendChannelMessage ::
  ChannelId ->
  Text ->
  EnvDatabaseDiscord s ()
sendChannelMessage c t = do
  res <- liftDiscord . restCall $ R.CreateMessage c t
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendReplyMessage@ sends the input message @t@ as a reply to the triggering
-- message @m@.
sendReplyMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s ()
sendReplyMessage m t = do
  let ref = MessageReference (Just (messageId m)) Nothing Nothing False
  res <- liftDiscord . restCall $ R.CreateMessageDetailed (messageChannelId m) (R.MessageDetailedOpts t False Nothing Nothing Nothing (Just ref) Nothing Nothing)
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendCustomReplyMessage@ sends the input message @t@ as a reply to a
-- provided message id @m@.
--
-- @fail'@ indicates whether the message should still send if the provided message id is invalid
sendCustomReplyMessage ::
  Message ->
  MessageId ->
  Bool ->
  Text ->
  EnvDatabaseDiscord s ()
sendCustomReplyMessage m mid fail' t = do
  let ref = MessageReference (Just mid) Nothing Nothing fail'
  res <- liftDiscord . restCall $ R.CreateMessageDetailed (messageChannelId m) (R.MessageDetailedOpts t False Nothing Nothing Nothing (Just ref) Nothing Nothing)
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendEmbedMessage@ sends the input message @t@ in the same channel as message
-- @m@ with an additional full Embed.
--
-- This is *really* janky. The library exposes *no way* to create a coloured embed through its main api,
-- so I'm having to manually reimplement the sending logic just to add this in.
-- If you suffer from nightmares, don't look in 'Sahasrara.Handler.Embed'. Nothing good lives there.
-- In the future, I may actually submit a PR to discord-haskell with a fix to allow colours properly.
sendEmbedMessage ::
  Embeddable e =>
  Message ->
  Text ->
  e ->
  EnvDatabaseDiscord s ()
sendEmbedMessage m = sendChannelEmbedMessage (messageChannelId m)

sendChannelEmbedMessage ::
  Embeddable e =>
  ChannelId ->
  Text ->
  e ->
  EnvDatabaseDiscord s ()
sendChannelEmbedMessage cid t e = do
  res <- liftDiscord . restCall $ R.CreateMessageDetailed cid (def {R.messageDetailedContent = t, R.messageDetailedEmbeds = Just [asEmbed e]})
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

sendEmbedInteraction ::
  Embeddable e =>
  Text ->
  e ->
  EnvDatabaseDiscord s MessageDetails
sendEmbedInteraction t e = return $ (messageDetailsBasic t) {messageDetailsEmbeds = Just [asEmbed e]}

sendEmbedInteractionWithButtons ::
  Embeddable e =>
  Text ->
  [Button] ->
  e ->
  EnvDatabaseDiscord s MessageDetails
sendEmbedInteractionWithButtons t bs e = return $
  (messageDetailsBasic t)
  { messageDetailsEmbeds = Just [asEmbed e],
    messageDetailsComponents = Just [ActionRowButtons bs]
  }

basicButton :: Text -> Text -> Text -> Text -> UserId -> Button
basicButton label emoji commandId buttonId userId =
  (mkButton label $ commandId <> " " <> buttonId <> " " <> (pack $ show userId))
  { buttonEmoji = Just (mkEmoji emoji) }

-- | @getChannel@ gets the relevant Channel object for a given 'ChannelId'
-- and 'MessageId', or returns an error ('RestCallErrorCode').
getChannel ::
  ChannelId ->
  EnvDatabaseDiscord s (Either RestCallErrorCode Channel)
getChannel cid = liftDiscord . restCall $ R.GetChannel cid

-- | @getMessage@ gets the relevant 'Message' object for a given 'ChannelId'
-- and 'MessageId', or returns an error ('RestCallErrorCode').
getMessage ::
  ChannelId ->
  MessageId ->
  EnvDatabaseDiscord s (Either RestCallErrorCode Message)
getMessage cid mid = liftDiscord . restCall $ R.GetChannelMessage (cid, mid)

-- | @reactToMessage@ reacts to the given message with the emoji specified
-- by the text input (see README.md from discord-haskell). Returns @()@ if
-- successful, else the error ('RestCallErrorCode').
reactToMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s (Either RestCallErrorCode ())
reactToMessage m e =
  liftDiscord . restCall $
    R.CreateReaction (messageChannelId m, messageId m) e

-- | @getReplyMessage@ returns the message being replied to (if applicable)
getReplyMessage :: Message -> EnvDatabaseDiscord s (Maybe Message)
getReplyMessage m = do
  let m' = messageReferencedMessage m
      mRef = messageReference m
  case m' of
    Just msg -> return $ Just msg
    Nothing -> case mRef of
      Nothing -> return Nothing
      Just mRef' -> maybeGetMessage (referenceChannelId mRef') (referenceMessageId mRef')
  where
    maybeGetMessage :: Maybe ChannelId -> Maybe MessageId -> EnvDatabaseDiscord s (Maybe Message)
    maybeGetMessage (Just cId) (Just mId) = do
      m' <- getMessage cId mId
      case m' of
        Left _ -> return Nothing
        Right msg -> return $ Just msg
    maybeGetMessage _ _ = return Nothing

-- | @getPrecedingMessage@ returns the message immediately above the provided message
getPrecedingMessage :: Message -> EnvDatabaseDiscord s (Maybe Message)
getPrecedingMessage m = do
  mlst <- liftDiscord . restCall $ R.GetChannelMessages (messageChannelId m) (1, R.BeforeMessage (messageId m))
  case mlst of
    Right mlst' ->
      return $ listToMaybe mlst'
    Left _ -> return Nothing

-- | @getMessageMember@ returns the message member object if it was sent from a Discord server,
-- or @Nothing@ if it was sent from a DM (or the API fails)
getMessageMember :: Message -> EnvDatabaseDiscord s (Maybe GuildMember)
getMessageMember m = gMM (messageGuildId m) m
  where
    maybeRight :: Either a b -> Maybe b
    maybeRight (Left _) = Nothing
    maybeRight (Right a) = Just a
    gMM :: Maybe GuildId -> Message -> EnvDatabaseDiscord s (Maybe GuildMember)
    gMM Nothing _ = return Nothing
    gMM (Just g') m' = do
      a <- liftDiscord $ restCall $ R.GetGuildMember g' (userId $ messageAuthor m')
      return $ maybeRight a

findGuild :: Message -> EnvDatabaseDiscord s (Maybe GuildId)
findGuild m = case messageGuildId m of
  Just a -> pure $ Just a
  Nothing -> do
    let chanId = messageChannelId m
    channel <- getChannel chanId
    case fmap channelGuild channel of
      Right a -> pure $ Just a
      Left _ -> pure Nothing

listGuildMembers :: Message -> EnvDatabaseDiscord s (Maybe [GuildMember])
listGuildMembers m = do
  gid <- findGuild m
  member <- getMessageMember m -- The R.GuildMembersTiming line breaks in DMs, so this determines if it's in a DM first
  case member of
    Just _ -> toMembers gid
    Nothing -> return Nothing
  where
    maybeRight :: Either a b -> Maybe b
    maybeRight (Left _) = Nothing
    maybeRight (Right a) = Just a
    toMembers :: Maybe GuildId -> EnvDatabaseDiscord s (Maybe [GuildMember])
    toMembers Nothing = return Nothing
    toMembers (Just gid) = do
      ms <- liftDiscord $ restCall $ R.ListGuildMembers gid $ R.GuildMembersTiming (Just 1000) Nothing
      return $ maybeRight ms

-- | Find an emoji from its name within a specific guild if it doesn't exist in the cache
-- Not exported, used by findEmoji and findGuildEmoji
getGuildEmoji :: Text -> GuildId -> EnvDatabaseDiscord s (Maybe Emoji)
getGuildEmoji ename gid = do
  cachedEmoji <- lookupEmojiCache ename
  case cachedEmoji of
    Just e -> pure $ Just e
    Nothing -> do
      guildResp <- liftDiscord $ restCall $ R.GetGuild gid
      case guildResp of
        Left _ -> pure Nothing
        Right guild -> do
          fillEmojiCache guild
          let emoji = filter ((ename ==) . emojiName) (guildEmojis guild)
          pure $ listToMaybe emoji

-- | search through all known guilds for an emoji with that name
findEmoji :: Text -> EnvDatabaseDiscord s (Maybe Emoji)
findEmoji ename = fmap msum (emojiServers >>= cacheToEmoji)
  where
    cacheToEmoji :: [GuildId] -> EnvDatabaseDiscord s [Maybe Emoji]
    cacheToEmoji ids = mapM (getGuildEmoji ename) ids
    emojiServers :: EnvDatabaseDiscord s [GuildId]
    emojiServers = do
      maybeServers <- liftIO $ lookupEnv "EMOJI_SERVERS"
      case maybeServers of
        Just x -> pure (read x)
        Nothing -> do
          cache <- liftDiscord readCache
          pure $ keys $ cacheGuilds cache

-- | Render an Emoji
formatEmoji :: Emoji -> Text
formatEmoji (Emoji (Just eId) eName _ _ _ anim) = prefix anim <> eName <> ":" <> pack (show eId) <> ">"
  where
    prefix (Just True) = "<a:"
    prefix _ = "<:"
formatEmoji (Emoji _ eName _ _ _ _) = eName

-- | Display an emoji as best as it can from its name
formatFromEmojiName :: Text -> EnvDatabaseDiscord s Text
formatFromEmojiName name = do
  emoji <- findEmoji name
  return $ maybe name formatEmoji emoji

-- | @toMention@ converts a user to its corresponding mention
toMention :: User -> Text
toMention = toMention' . userId

-- | @toMention'@ converts a user ID to its corresponding mention
toMention' :: UserId -> Text
toMention' u = "<@!" <> pack (show u) <> ">"

-- | @fromMention@ converts some text into what could be a userid (which isn't checked
-- for correctness above getting rid of triangle brackets, '@', and the optional '!')
fromMention :: Text -> Maybe UserId
fromMention = fromMentionStr . unpack

-- | Try to get the userid from a given string.
fromMentionStr :: String -> Maybe UserId
fromMentionStr user
  | length user < 4 || head user /= '<' || last user /= '>' || (head . tail) user /= '@' || (head stripToNum /= '!' && (not . isDigit) (head stripToNum)) = Nothing
  | all isDigit (tail stripToNum) = Just $ if head stripToNum == '!' then read (tail stripToNum) else read stripToNum
  | otherwise = Nothing
  where
    stripToNum = (init . tail . tail) user

-- | Data types for different time formats.
data TimeFormat = Default | ShortTime | LongTime | ShortDate | LongDate | ShortDateTime | LongDateTime | Relative deriving (Show, Enum, Eq)

-- | Turn some UTCTime into the given TimeFormat.
toTimestamp' :: TimeFormat -> UTCTime -> Text
toTimestamp' format t = "<t:" <> pack (show $ toUtcSeconds t) <> toSuffix format <> ">"
  where
    toUtcSeconds :: UTCTime -> Integer
    toUtcSeconds = truncate . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    toSuffix :: TimeFormat -> Text
    toSuffix Default = ""
    toSuffix ShortTime = ":t"
    toSuffix LongTime = ":T"
    toSuffix ShortDate = ":d"
    toSuffix LongDate = ":D"
    toSuffix ShortDateTime = ":f"
    toSuffix LongDateTime = ":F"
    toSuffix Relative = ":R"

-- | Turn some UTCTime into the default time format.
toTimestamp :: UTCTime -> Text
toTimestamp = toTimestamp' Default

-- | Turn some UTCTime into a relative time format
toRelativeTime :: UTCTime -> Text
toRelativeTime = toTimestamp' Relative

-- | Create a link to a message when given the server id, channel id, and
-- message id.
getMessageLink :: GuildId -> ChannelId -> MessageId -> Text
getMessageLink g c m = pack $ "https://discord.com/channels/" ++ show g ++ "/" ++ show c ++ "/" ++ show m

-- | The data types of different formatting options.
--
-- Note that repeatedly applying certain formatting options (such as `Italics`,
-- `Code`, and a few others) will result in other formats.
data Format = Bold | Underline | Strikethrough | Italics | Code | CodeBlock
  deriving (Show, Eq)

-- | Format some `a` (that can be turned into a string format) with the given
-- formatting option.
formatInput :: (IsString a, Show b, Semigroup a) => Format -> b -> a
formatInput f b = formatText f (fromString $ show b)

-- | Format the given string-like object with the given format.
formatText :: (IsString a, Semigroup a) => Format -> a -> a
formatText Bold s = "**" <> s <> "**"
formatText Underline s = "__" <> s <> "__"
formatText Strikethrough s = "~~" <> s <> "~~"
formatText Italics s = "*" <> s <> "*"
formatText Code s = "`" <> s <> "`"
formatText CodeBlock s = "```" <> s <> "```"

-- | Get the `Word64` within a `Snowflake`.
extractFromSnowflake :: Snowflake -> Word64
extractFromSnowflake (Snowflake w) = w

-- | When given an application id, an optional server id, and a
-- CreateApplicationCommand object, create the application command.
createApplicationCommand :: ApplicationId -> Maybe GuildId -> CreateApplicationCommand -> DiscordHandler ApplicationCommand
createApplicationCommand aid gid cac = do
  res <- createAppComm
  case res of
    Left e -> throw $ InteractionException $ "Failed to create application command :" ++ show e
    Right a -> return a
  where
    createAppComm = case gid of
      Nothing -> restCall $ R.CreateGlobalApplicationCommand aid cac
      Just gid' -> restCall $ R.CreateGuildApplicationCommand aid gid' cac

-- | Remove all application commands that are active (optionally in the given
-- server) that aren't in the given list.
removeApplicationCommandsNotInList :: ApplicationId -> Maybe GuildId -> [ApplicationCommandId] -> DiscordHandler ()
removeApplicationCommandsNotInList aid gid aciToKeep = do
  allACs' <- getAppComm
  case allACs' of
    Left _ -> throw $ InteractionException "Failed to get all application commands."
    Right aacs ->
      let allACs = applicationCommandId <$> aacs
       in mapM_ deleteAppComm (allACs \\ aciToKeep)
  where
    (getAppComm, deleteAppComm) = case gid of
      Nothing -> (restCall $ R.GetGlobalApplicationCommands aid, restCall . R.DeleteGlobalApplicationCommand aid)
      Just gid' -> (restCall $ R.GetGuildApplicationCommands aid gid', restCall . R.DeleteGuildApplicationCommand aid gid')

-- | Defer an interaction response, extending the window of time to respond to
-- 15 minutes (from 3 seconds).
interactionResponseDefer :: Interaction -> EnvDatabaseDiscord s ()
interactionResponseDefer i = do
  res <- liftDiscord $ restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) InteractionResponseDeferChannelMessage
  case res of
    Left _ -> throw $ InteractionException "Failed to defer interaction."
    Right _ -> return ()

-- | Defer an interaction response, extending the window of time to respond to
-- 15 minutes (from 3 seconds).
--
-- Used when updating a component message. Does not show that the bot is
-- thinking about the interaction.
interactionResponseDeferUpdateMessage :: Interaction -> EnvDatabaseDiscord s ()
interactionResponseDeferUpdateMessage i = do
  res <- liftDiscord $ restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) InteractionResponseDeferUpdateMessage
  case res of
    Left _ -> throw $ InteractionException "Failed to defer interaction."
    Right _ -> return ()

-- | Respond to the given interaction with the given text.
interactionResponseMessage :: Interaction -> Text -> EnvDatabaseDiscord s ()
interactionResponseMessage i t = interactionResponseCustomMessage i (messageDetailsBasic t)

-- | Respond to the given interaction with a custom messages object.
interactionResponseCustomMessage :: Interaction -> MessageDetails -> EnvDatabaseDiscord s ()
interactionResponseCustomMessage i t = do
  res <- liftDiscord $ restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) (InteractionResponseChannelMessage (convertMessageFormatInteraction t))
  case res of
    Left _ -> throw $ InteractionException "Failed to respond to interaction."
    Right _ -> return ()

-- | Respond to the given interaction by updating the component's message.
interactionResponseComponentsUpdateMessage :: Interaction -> MessageDetails -> EnvDatabaseDiscord s ()
interactionResponseComponentsUpdateMessage i t = do
  res <- liftDiscord $ restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) (InteractionResponseUpdateMessage (convertMessageFormatInteraction t))
  case res of
    Left _ -> throw $ InteractionException "Failed to respond to interaction with components update."
    Right _ -> return ()

-- | Respond to the given interaction by sending a list of choices back.
interactionResponseAutocomplete :: Interaction -> InteractionResponseAutocomplete -> EnvDatabaseDiscord s ()
interactionResponseAutocomplete i ac = do
  res <- liftDiscord $ restCall $ R.CreateInteractionResponse (interactionId i) (interactionToken i) (InteractionResponseAutocompleteResult ac)
  case res of
    Left _ -> throw $ InteractionException "Failed to respond to interaction with autocomplete response."
    Right _ -> return ()
