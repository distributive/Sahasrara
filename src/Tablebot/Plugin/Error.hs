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
module Tablebot.Plugin.Error where

-- | @BotError@ is the type for errors caught in TableBot.
data BotError
  = UnnamedError String String
  | IndexOutOfBoundsError Int (Int, Int)
  | RandomError String

-- | @formatUserError@ takes an error's name and message and makes it pretty for
-- Discord.
formatUserError :: String -> String -> String
formatUserError name message =
  ":warning: **" ++ name ++ "** :warning:\n"
    ++ "An error was encountered while resolving your command:\n"
    ++ "> `"
    ++ message
    ++ "`"

-- | @ErrorInfo@ packs the info for each error into one data type. This allows
-- each error type to be defined in one block (as opposed to errorName being
-- defined for each error type _then_ errorMsg being defined for each type).
data ErrorInfo = ErrorInfo {name :: String, msg :: String}

-- | @errorInfo@ takes a BotError and converts it into an ErrorInfo struct.
errorInfo :: BotError -> ErrorInfo
errorName :: BotError -> String
errorName = name . errorInfo

errorMsg :: BotError -> String
errorMsg = msg . errorInfo

showError :: BotError -> String
showError e = (errorName e) ++ ": " ++ (errorMsg e)

showUserError :: BotError -> String
showUserError e = formatUserError (errorName e) (errorMsg e)

errorInfo (UnnamedError name msg) = ErrorInfo name msg
errorInfo (IndexOutOfBoundsError index (a, b)) =
  ErrorInfo
    "IndexOutOfBoundsError"
    $ "Index value of " ++ (show index) ++ " is not in the valid range [" ++ (show a) ++ ", " ++ (show b) ++ "]."
errorInfo (RandomError msg) = ErrorInfo "RandomError" msg
