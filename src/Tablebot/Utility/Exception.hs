-- |
-- Module      : Tablebot.Utility.Exception
-- Description : Helpers for error handling in plugins.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Helpers for error handling in plugins.
module Tablebot.Utility.Exception
  ( BotException (..),
    throwBot,
    catchBot,
    transformException,
    transformExceptionConst,
    showError,
    showUserError,
    embedError,
  )
where

import Control.Monad.Exception (Exception, MonadException, catch, throw)
import Data.List (intercalate)
import Data.Text (pack)
import Discord.Internal.Types
import Tablebot.Utility.Embed
import Tablebot.Utility.Types (DiscordColour (..))

-- | @BotException@ is the type for errors caught in TableBot.
-- Declare new errors here, and define them at the bottom of the file.
data BotException
  = GenericException String String
  | MessageSendException String
  | ParserException String String
  | IndexOutOfBoundsException Int (Int, Int)
  | RandomException String
  | EvaluationException String [String]
  | IOException String
  | NetrunnerException String
  deriving (Show, Eq)

instance Exception BotException

-- | Aliases for throw and catch that enforce the exception type.
throwBot :: MonadException m => BotException -> m a
throwBot = throw

catchBot :: MonadException m => m a -> (BotException -> m a) -> m a
catchBot = catch

-- | @transformException@ takes a computation m that may fail, catches any
-- exception it throws, and transforms it into a new one with transformer.
transformException :: MonadException m => m a -> (BotException -> BotException) -> m a
transformException m transformer = m `catchBot` (throwBot . transformer)

-- | @transformExceptionConst@ takes a computation m that may fail and replaces
-- any exception it throws with the constant exception e.
transformExceptionConst :: MonadException m => m a -> BotException -> m a
transformExceptionConst m e = m `catchBot` \_ -> throwBot e

-- | @errorEmoji@ defines a Discord emoji in plaintext for use in error outputs.
errorEmoji :: String
errorEmoji = ":warning:"

-- | @formatUserError@ takes an error's name and message and makes it pretty for
-- Discord.
formatUserError :: String -> String -> String
formatUserError name' message =
  errorEmoji ++ " **" ++ name' ++ "** " ++ errorEmoji ++ "\n"
    ++ "An error was encountered while resolving your command:\n"
    ++ "> `"
    ++ message
    ++ "`"

-- | @ErrorInfo@ packs the info for each error into one data type. This allows
-- each error type to be defined in one block (as opposed to errorName being
-- defined for each error type _then_ errorMsg being defined for each type).
data ErrorInfo = ErrorInfo {name :: String, msg :: String}

-- | @errorName@ generates the name of a given error.
errorName :: BotException -> String
errorName = name . errorInfo

-- | @errorMsg@ generates the message of a given error.
errorMsg :: BotException -> String
errorMsg = msg . errorInfo

-- | @showError@ generates the command line output of a given error.
showError :: BotException -> String
showError e = errorName e ++ ": " ++ errorMsg e

-- | @showUserError@ generates a user-facing error for outputting to Discord.
showUserError :: BotException -> String
showUserError e = formatUserError (errorName e) (errorMsg e)

-- | @embedError@ takes an error and makes it into an embed.
embedError :: BotException -> Embed
embedError e =
  addTitle (pack $ errorEmoji ++ " **" ++ errorName e ++ "** " ++ errorEmoji) $
    addColour Red $
      simpleEmbed (pack $ errorMsg e)

-- | @errorInfo@ takes a BotException and converts it into an ErrorInfo struct.
errorInfo :: BotException -> ErrorInfo

-- | Add new errors here. Do not modify anything above this line except to
-- declare new errors in the definition of BotException.
errorInfo (GenericException name' msg') = ErrorInfo name' msg'
errorInfo (MessageSendException msg') = ErrorInfo "MessageSendException" msg'
errorInfo (ParserException title msg') = ErrorInfo title msg'
errorInfo (IndexOutOfBoundsException index (a, b)) =
  ErrorInfo
    "IndexOutOfBoundsException"
    $ "Index value of " ++ show index ++ " is not in the valid range [" ++ show a ++ ", " ++ show b ++ "]."
errorInfo (RandomException msg') = ErrorInfo "RandomException" msg'
errorInfo (EvaluationException msg' locs) = ErrorInfo "EvaluationException" $ msg' ++ if null locs then "" else ".\nException evaluation stack:\n" ++ str
  where
    l = length locs
    ls = reverse $ take 3 locs
    fs = reverse $ drop (l - 3) locs
    connectVs vs = "in `" ++ intercalate "`\nin `" vs ++ "`"
    -- connectVs vs = "in `" ++ foldr (++) "`" (intersperse "`\nin `" vs)
    str =
      if l > 6
        then connectVs fs ++ "\n...\n" ++ connectVs ls
        else connectVs (reverse locs)
errorInfo (IOException msg') = ErrorInfo "IOException" msg'
errorInfo (NetrunnerException msg') = ErrorInfo "NetrunnerException" msg'
