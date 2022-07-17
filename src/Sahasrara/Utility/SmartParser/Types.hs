{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Sahasrara.Utility.SmartParser.Types
-- Description : Some of the types or typeclasses for smart parsers.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Sahasrara.Utility.SmartParser.Types where

import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Discord.Interactions
import Discord.Types
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Sahasrara.Utility.Discord (getChannel)
import Sahasrara.Utility.Types
import Text.Megaparsec (observing)

-- | The type class representing some data we can extract data from.
-- Needed for things like getting a GuildMember, message id, guild id.
--
-- Only defined for Message and Interaction.
class Context a where
  contextUserId :: a -> UserId
  contextGuildId :: a -> EnvDatabaseDiscord s (Maybe GuildId)
  contextMember :: a -> Maybe GuildMember
  contextMessageId :: a -> Maybe MessageId

newtype SenderUserId = SenderUserId UserId deriving (Show, Eq)

instance Context Message where
  contextUserId = userId . messageAuthor
  contextGuildId m = case messageGuildId m of
    Just a -> pure $ Just a
    Nothing -> do
      let chanId = messageChannelId m
      channel <- getChannel chanId
      case fmap channelGuild channel of
        Right a -> pure $ Just a
        Left _ -> pure Nothing
  contextMember = messageMember
  contextMessageId = return . messageId

instance Context Interaction where
  -- this is safe to do because we are guaranteed to get either a user or a member
  contextUserId i = maybe 0 userId (either memberUser Just mor)
    where
      (MemberOrUser mor) = interactionUser i
  contextGuildId i = return $ interactionGuildId i
  contextMember i = case interactionUser i of
    (MemberOrUser (Left m)) -> return m
    (MemberOrUser (Right _)) -> Nothing
  contextMessageId InteractionComponent {interactionMessage = m} = return $ messageId m
  contextMessageId InteractionApplicationCommand {applicationCommandData = ApplicationCommandDataMessage {..}} = return applicationCommandDataTargetMessageId
  contextMessageId _ = Nothing

-- | Custom infix operator to replace the error of a failing parser (regardless
-- of parser position) with a user given error message.
--
-- Has some effects on other error parsing. Use if you want the error you give
-- to be the one that is reported (unless this is used at a higher level.)
--
-- Overwrites/overpowers WithError errors.
(<??>) :: Parser a -> String -> Parser a
(<??>) p s = do
  r <- observing p
  case r of
    Left _ -> fail s
    Right a -> return a

-- | @Quoted a@ defines an input of type @a@ that is contained within quotes.
newtype Quoted a = Qu {quote :: a} deriving (Show)

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput a = ROI {unROI :: a}

-- | @Exactly s@ defines an input exactly matching @s@ and nothing else.
data Exactly (s :: Symbol) = Ex

-- | @RestOfInput a@ parses the rest of the input, giving a value of type @a@.
newtype RestOfInput1 a = ROI1 a

-- | @WithError err x@ parses an @x@, reporting @err@ if the parsing of @x@
-- fails.
newtype WithError (err :: Symbol) x = WErr x

-- | Labelled value for use with smart commands.
--
-- This is for use with slash commands, where there is a name and description
-- required.
newtype Labelled (name :: Symbol) (desc :: Symbol) a = Labelled {unLabel :: a}

-- | Easily make a labelled value.
labelValue :: forall n d a. a -> Labelled n d a
labelValue = Labelled @n @d

-- | Get the name and description of a labelled value.
getLabelValues :: forall n d a. (KnownSymbol n, KnownSymbol d) => Proxy (Labelled n d a) -> (Text, Text)
getLabelValues _ = (pack (symbolVal (Proxy :: Proxy n)), pack (symbolVal (Proxy :: Proxy d)))
