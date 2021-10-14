-- |
-- Module      : Tablebot.Util.Error
-- Description : A plugin for error types.
-- Copyright   : (c) Amelie WD, Sam Coy 2021
-- License     : MIT
-- Maintainer  : tablebot@ameliewd.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A plugin for error handling.
module Tablebot.Plugin.Error
  ( BotException (..),
    showError,
    showUserError,
    embedError,
  )
where

import Data.Text (pack)
import Discord.Internal.Types
import Tablebot.Plugin.Embed
import Tablebot.Plugin.Types (DiscordColour (..))

import Control.Monad.Exception (Exception)

-- | @BotException@ is the type for errors caught in TableBot.
-- Declare new errors here, and define them at the bottom of the file.
data BotException
  = GenericException String String
  | ParseException String
  | IndexOutOfBoundsException Int (Int, Int)
  | RandomException String
  deriving (Show, Eq)

instance Exception BotException

-- | @errorEmoji@ defines a Discord emoji in plaintext for use in error outputs.
errorEmoji :: String
errorEmoji = ":warning:"

-- | @formatUserError@ takes an error's name and message and makes it pretty for
-- Discord.
formatUserError :: String -> String -> String
formatUserError name message =
  errorEmoji ++ " **" ++ name ++ "** " ++ errorEmoji ++ "\n"
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
showError e = (errorName e) ++ ": " ++ (errorMsg e)

-- | @showUserError@ generates a user-facing error for outputting to Discord.
showUserError :: BotException -> String
showUserError e = formatUserError (errorName e) (errorMsg e)

-- | @embedError@ takes an error and makes it into an embed.
embedError :: BotException -> Embed
embedError e =
  addTitle (pack $ errorEmoji ++ " **" ++ (errorName e) ++ "** " ++ errorEmoji) $
    addColour Red $
      simpleEmbed (pack $ errorMsg e)

-- | @errorInfo@ takes a BotException and converts it into an ErrorInfo struct.
errorInfo :: BotException -> ErrorInfo

-- | Add new errors here. Do not modify anything above this line except to
-- declare new errors in the definition of BotException.
errorInfo (GenericException name msg) = ErrorInfo name msg
errorInfo (ParseException msg) = ErrorInfo "ParseException" msg
errorInfo (IndexOutOfBoundsException index (a, b)) =
  ErrorInfo
    "IndexOutOfBoundsException"
    $ "Index value of " ++ (show index) ++ " is not in the valid range [" ++ (show a) ++ ", " ++ (show b) ++ "]."
errorInfo (RandomException msg) = ErrorInfo "RandomException" msg
