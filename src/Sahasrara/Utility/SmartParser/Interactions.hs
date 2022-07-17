{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Sahasrara.Utility.Interactions
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.
module Sahasrara.Utility.SmartParser.Interactions where

import Control.Monad.Exception (MonadException (catch))
import Data.Default (Default (def))
import Data.Proxy (Proxy (..))
import Data.Scientific
import Data.Text (Text, pack)
import Discord.Interactions
import Discord.Types
import GHC.OldList (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Sahasrara.Internal.Handler.Command (parseValue)
import Sahasrara.Utility.Discord (interactionResponseComponentsUpdateMessage, interactionResponseCustomMessage)
import Sahasrara.Utility.Exception (BotException (InteractionException, ParserException), catchBot, embedError, throwBot)
import Sahasrara.Utility.Parser
import Sahasrara.Utility.SmartParser.SmartParser (PComm (..))
import Sahasrara.Utility.SmartParser.Types
import Sahasrara.Utility.Types
import Text.Megaparsec (MonadParsec (eof))

-- | Creates both the slash command creation data structure and the parser for
-- the command, and creates the EnvApplicationCommandRecv for the command by
-- combining them.
--
-- Takes the name and description for a slash command, and its function.
makeApplicationCommandPair :: forall t s. (MakeAppComm t, ProcessAppComm t s) => Text -> Text -> t -> Maybe (EnvApplicationCommandRecv s)
makeApplicationCommandPair name desc f = do
  cac <- makeSlashCommand name desc (Proxy :: Proxy t)
  return $ ApplicationCommandRecv cac (processAppComm f)

-- | Make the creation data structure for a slash command when given a proxy for
-- a function's type.
makeSlashCommand :: (MakeAppComm t) => Text -> Text -> Proxy t -> Maybe CreateApplicationCommand
makeSlashCommand name desc p =
  createChatInput name desc >>= \cac ->
    return $
      cac
        { createOptions = Just $ OptionsValues $ makeAppComm p
        }

-- | Create a series of command option values from the given types.
--
-- This is making the arguments for a text input/slash command from
-- a proxy of the given function.
class MakeAppComm commandty where
  makeAppComm :: Proxy commandty -> [OptionValue]

-- As a base case, no more arguments
instance {-# OVERLAPPING #-} MakeAppComm (EnvDatabaseDiscord s MessageDetails) where
  makeAppComm _ = []

-- If there is a way to get an argument from a `ty`, then get that arg and continue recursion.
instance {-# OVERLAPPABLE #-} (MakeAppComm mac, MakeAppCommArg ty) => MakeAppComm (ty -> mac) where
  makeAppComm _ = makeAppCommArg (Proxy :: Proxy ty) : makeAppComm (Proxy :: Proxy mac)

-- we don't get the sender user id from the command itself, so ignore it
instance {-# OVERLAPPABLE #-} (MakeAppComm mac) => MakeAppComm (SenderUserId -> mac) where
  makeAppComm _ = makeAppComm (Proxy :: Proxy mac)

-- | From a single value, make an argument for a slash command command.
class MakeAppCommArg commandty where
  makeAppCommArg :: Proxy commandty -> OptionValue

-- | Create a labelled text argument. By default it is required and does not
-- have autocompeletion.
instance (KnownSymbol name, KnownSymbol desc) => MakeAppCommArg (Labelled name desc Text) where
  makeAppCommArg l = OptionValueString n d True (Left False)
    where
      (n, d) = getLabelValues l

-- | Create a labelled integer argument. By default it is required and does not
-- have autocompeletion, and does not have bounds.
instance (KnownSymbol name, KnownSymbol desc) => MakeAppCommArg (Labelled name desc Integer) where
  makeAppCommArg l = OptionValueInteger n d True (Left False) Nothing Nothing
    where
      (n, d) = getLabelValues l

-- | Create a labelled scientific argument. By default it is required and does not
-- have autocompeletion, and does not have bounds.
instance (KnownSymbol name, KnownSymbol desc) => MakeAppCommArg (Labelled name desc Scientific) where
  makeAppCommArg l = OptionValueNumber n d True (Left False) Nothing Nothing
    where
      (n, d) = getLabelValues l

-- | Create a labelled argument that is optional.
instance (KnownSymbol name, KnownSymbol desc, MakeAppCommArg (Labelled name desc t)) => MakeAppCommArg (Labelled name desc (Maybe t)) where
  makeAppCommArg _ =
    (makeAppCommArg (Proxy :: Proxy (Labelled name desc t)))
      { optionValueRequired = False
      }

-- | When quoted text is required, just fake it and get a sub layer.
instance (KnownSymbol name, KnownSymbol desc, MakeAppCommArg (Labelled name desc t)) => MakeAppCommArg (Labelled name desc (Quoted t)) where
  makeAppCommArg _ = makeAppCommArg (Proxy :: Proxy (Labelled name desc t))

-- As a base case, send the message produced

-- | Process an application command when given a function/value.
--
-- `s` is the context of the environment.
class ProcessAppComm commandty s where
  processAppComm :: commandty -> Interaction -> EnvDatabaseDiscord s ()

-- When left with just a MessageDetails, just send the message as an
-- interaction response.
instance {-# OVERLAPPING #-} ProcessAppComm (EnvDatabaseDiscord s MessageDetails) s where
  processAppComm comm i = comm >>= interactionResponseCustomMessage i

-- If there is already an interaction in this function call, apply it and
-- recurse.
instance {-# OVERLAPPABLE #-} (ProcessAppComm pac s) => ProcessAppComm (Interaction -> pac) s where
  processAppComm comm i = processAppComm (comm i) i

-- This is the main recursion case.
--
-- If the argument is a ProcessAppCommArg, then parse it and recurse.
instance {-# OVERLAPPABLE #-} (ProcessAppCommArg ty s, ProcessAppComm pac s) => ProcessAppComm (ty -> pac) s where
  processAppComm comm i@InteractionApplicationCommand {applicationCommandData = ApplicationCommandDataChatInput {optionsData = opts}} = do
    t <- processAppCommArg (getVs opts)
    processAppComm (comm t) i
    where
      getVs (Just (OptionsDataValues vs)) = vs
      getVs _ = []
  processAppComm _ _ = throwBot $ InteractionException "could not process args to application command"

-- one specific implementation case when we want to parse out a user id.
instance {-# OVERLAPPABLE #-} (ProcessAppComm pac s) => ProcessAppComm (SenderUserId -> pac) s where
  processAppComm comm i@InteractionApplicationCommand {interactionUser = MemberOrUser u} =
    case getUser of
      Nothing -> throwBot $ InteractionException "could not process args to application command"
      Just uid -> processAppComm (comm (SenderUserId uid)) i
    where
      getUser = userId <$> either memberUser Just u
  processAppComm _ _ = throwBot $ InteractionException "could not process args to application command"

-- | Process an argument for an application command.
--
-- Given a type `t`, parse a value of that type from the given list of option
-- values.
class ProcessAppCommArg t s where
  processAppCommArg :: [OptionDataValue] -> EnvDatabaseDiscord s t

-- | Given a string, find the first option value with that name in the list,
-- returning Nothing if none is found.
getValue :: String -> [OptionDataValue] -> Maybe OptionDataValue
getValue t = find ((== pack t) . optionDataValueName)

-- | Tries to extract an integer from a given option value.
integerFromOptionValue :: OptionDataValue -> Maybe Integer
integerFromOptionValue OptionDataValueInteger {optionDataValueInteger = Right i} = Just i
integerFromOptionValue _ = Nothing

-- | Tries to extract a scientific number from a given option value.
scientificFromOptionValue :: OptionDataValue -> Maybe Scientific
scientificFromOptionValue OptionDataValueNumber {optionDataValueNumber = Right i} = Just i
scientificFromOptionValue _ = Nothing

-- | Tries to extract a string from a given option value.
stringFromOptionValue :: OptionDataValue -> Maybe Text
stringFromOptionValue OptionDataValueString {optionDataValueString = Right i} = Just i
stringFromOptionValue _ = Nothing

-- there are a number of missing slash command argument types missing here, which I've not added yet.
-- we can add ids of various sorts

-- extract a string of the given type from the arguments
instance (KnownSymbol name) => ProcessAppCommArg (Labelled name desc Text) s where
  processAppCommArg is = case getValue (symbolVal (Proxy :: Proxy name)) is of
    Just (OptionDataValueString _ (Right t)) -> return $ labelValue t
    _ -> throwBot $ InteractionException "could not find required parameter"

-- extract an integer of the given type from the arguments
instance (KnownSymbol name) => ProcessAppCommArg (Labelled name desc Integer) s where
  processAppCommArg is = case getValue (symbolVal (Proxy :: Proxy name)) is of
    Just (OptionDataValueInteger _ (Right i)) -> return $ labelValue i
    _ -> throwBot $ InteractionException "could not find required parameter"

-- extract a scientific number of the given type from the arguments
instance (KnownSymbol name) => ProcessAppCommArg (Labelled name desc Scientific) s where
  processAppCommArg is = case getValue (symbolVal (Proxy :: Proxy name)) is of
    Just (OptionDataValueNumber _ (Right i)) -> return $ labelValue i
    _ -> throwBot $ InteractionException "could not find required parameter"

-- extract a quote of the given type from the arguments
instance (KnownSymbol name, KnownSymbol desc, ProcessAppCommArg (Labelled name desc t) s) => ProcessAppCommArg (Labelled name desc (Quoted t)) s where
  processAppCommArg is = processAppCommArg @(Labelled name desc t) is >>= \(Labelled a) -> return (labelValue (Qu a))

-- extract an optional data type from the arguments
instance (KnownSymbol name, ProcessAppCommArg (Labelled name desc t) s) => ProcessAppCommArg (Labelled name desc (Maybe t)) s where
  processAppCommArg is = do
    let result = processAppCommArg is :: EnvDatabaseDiscord s (Labelled name desc t)
    ( do
        (Labelled l) <- result
        return (labelValue (Just l))
      )
      `catchBot` const (return $ labelValue Nothing)

-- | Given a function that can be processed to create a parser, create an action
-- for it using the helper. Uses `parseComm` to generate the required parser.
--
-- Components use a unique string as their identifier. We can use this to
-- run the normal command parser on, hence the use of PComm.
--
-- If the boolean is False, a reply is sent to the interaction message. If the
-- boolean is True, the original message is updated.
--
-- For more information, check the helper `processComponentInteraction'`.
processComponentInteraction :: (PComm f s Interaction MessageDetails) => f -> Bool -> Interaction -> EnvDatabaseDiscord s ()
processComponentInteraction f = processComponentInteraction' (parseComm f)

-- | Given a parser that, when run, returns a function taking an interaction
-- and returns a database action on some MessageDetails, run the action.
--
-- If the boolean is true, the message the component is from is updated. Else,
-- a message is sent as the interaction response.
--
-- The format of the Text being given should be of space separated values,
-- similar to the command structure.
processComponentInteraction' :: Parser (Interaction -> EnvDatabaseDiscord s MessageDetails) -> Bool -> Interaction -> EnvDatabaseDiscord s ()
processComponentInteraction' compParser updateOriginal i@InteractionComponent {componentData = idc} = errorCatch $ do
  let componentSend
        | updateOriginal = interactionResponseComponentsUpdateMessage i
        | otherwise = interactionResponseCustomMessage i
  action <- parseValue (skipSpace *> compParser) (componentDataCustomId idc) >>= ($ i)
  componentSend action
  where
    catchParserException e@(ParserException _ _) = interactionResponseCustomMessage i $ (messageDetailsBasic "something (likely) went wrong when processing a component interaction") {messageDetailsEmbeds = Just [embedError (e :: BotException)]}
    catchParserException e = interactionResponseCustomMessage i $ (messageDetailsBasic "") {messageDetailsEmbeds = Just [embedError (e :: BotException)]}
    errorCatch = (`catch` catchParserException)
processComponentInteraction' _ _ _ = throwBot $ InteractionException "could not process component interaction"

-- | Function to only allow use of an interaction if the requestor matches
-- a Snowflake at the beginning of the input. This uses a helper, and by default
-- sends an ephermeral message with the text "You don't have permission to use
-- this component."
--
-- Helper is `onlyAllowRequestor'`.
onlyAllowRequestor :: forall f. (PComm f () Interaction MessageDetails) => f -> Parser (Interaction -> DatabaseDiscord MessageDetails)
onlyAllowRequestor =
  onlyAllowRequestor'
    ( (messageDetailsBasic "You don't have permission to use this component.") {messageDetailsFlags = Just $ InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral]}
    )

-- | Take a message to send when a user that is not the one that created a
-- component, and then parse out a user id, and then get the interaction
-- requestor's userid, check if they match, and if they don't then send a
-- message. Regardless, parse out the given function. If it _does_ match, run
-- the parsed function.
--
-- Adds eof to the end to ensure all the data is parsed.
onlyAllowRequestor' :: forall f. (PComm f () Interaction MessageDetails) => MessageDetails -> f -> Parser (Interaction -> DatabaseDiscord MessageDetails)
onlyAllowRequestor' msg f = do
  pre <- parseComm prefunc
  f' <- parseComm @f @() @Interaction @MessageDetails f
  parseComm
    ( \i -> do
        isEqual <- pre i
        case isEqual of
          Nothing -> f' i
          Just d -> return d
    )
    <* eof
  where
    prefunc :: UserId -> SenderUserId -> Interaction -> DatabaseDiscord (Maybe MessageDetails)
    prefunc uid (SenderUserId u) i =
      if uid == u
        then return Nothing
        else
          interactionResponseCustomMessage
            i
            msg
            >> return (Just def)
