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
  ( BotError (..),
    showError,
    showUserError,
    embedError,
  )
where

import Data.Text (pack)
import Discord.Internal.Types
import Tablebot.Plugin.Embed
import Tablebot.Plugin.Types (DiscordColour (..))

-- | @BotError@ is the type for errors caught in TableBot.
-- Declare new errors here, and define them at the bottom of the file.
data BotError
  = UnnamedError String String
  | ParserError String
  | IndexOutOfBoundsError Int (Int, Int)
  | RandomError String

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
errorName :: BotError -> String
errorName = name . errorInfo

-- | @errorMsg@ generates the message of a given error.
errorMsg :: BotError -> String
errorMsg = msg . errorInfo

-- | @showError@ generates the command line output of a given error.
showError :: BotError -> String
showError e = (errorName e) ++ ": " ++ (errorMsg e)

-- | @showUserError@ generates a user-facing error for outputting to Discord.
showUserError :: BotError -> String
showUserError e = formatUserError (errorName e) (errorMsg e)

-- | @embedError@ takes an error and makes it into an embed.
embedError :: BotError -> Embed
embedError e =
  addTitle (pack $ errorEmoji ++ " **" ++ (errorName e) ++ "** " ++ errorEmoji) $
    addColour Red $
      simpleEmbed (pack $ errorMsg e)

-- | @errorInfo@ takes a BotError and converts it into an ErrorInfo struct.
errorInfo :: BotError -> ErrorInfo

-- | Add new errors here. Do not modify anything above this line except to
-- declare new errors in the definition of BotError.
errorInfo (UnnamedError name msg) = ErrorInfo name msg
errorInfo (ParserError msg) = ErrorInfo "ParserError" msg
errorInfo (IndexOutOfBoundsError index (a, b)) =
  ErrorInfo
    "IndexOutOfBoundsError"
    $ "Index value of " ++ (show index) ++ " is not in the valid range [" ++ (show a) ++ ", " ++ (show b) ++ "]."
errorInfo (RandomError msg) = ErrorInfo "RandomError" msg
