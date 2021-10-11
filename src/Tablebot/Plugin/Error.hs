-- |
-- Module      : Tablebot.Util.Error
-- Description : A plugin for error types.
-- Copyright   : (c) Amelie WD 2021
-- License     : MIT
-- Maintainer  : tablebot@ameliewd.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A plugin for error handling.
module Tablebot.Plugin.Error where

--------------------------------------------------------------------------------

-- | @BotError@ is the universal typeclass for errors within TableBot.
-- @showError@ maps a BotError to a command-line error message.
-- @showUserError@ maps a BotError to a user-facing Discord error message.
class BotError e where
  showError :: e -> String
  showUserError :: e -> String

-- | @formatUserError@ takes an error's name and message and makes it pretty for
-- Discord.
formatUserError :: String -> String -> String
formatUserError name message =
  ":warning: **" ++ name ++ "** :warning:\n" ++
  "An error was encountered while resolving your command:\n" ++
  "> `" ++ message ++ "`"

--------------------------------------------------------------------------------

-- | @SimpleError@ is a simple general-purpose data type for BotErrors.
-- It specifies only a name of the error and a generic message.
data SimpleError = SimpleError { simpleErrorName :: String
                               , simpleErrorMsg :: String
                               }

instance BotError SimpleError where
  showError e = (simpleErrorName e) ++ ": " ++ (simpleErrorMsg e)
  showUserError e = formatUserError (simpleErrorName e) (simpleErrorMsg e)

--------------------------------------------------------------------------------

-- | @IndexOutOfBoundsError@ is a template for such errors.
-- It specifies the erronous index and the valid range (inclusively).
data IndexOutOfBoundsError = IndexOutOfBoundsError { errorIndex :: Int
                                                   , errorRange :: (Int, Int)
                                                   }
instance BotError IndexOutOfBoundsError where
  showError e =
    "IndexOutOfBoundsError detected with an index of " ++
    (show $ errorIndex e) ++ ". Valid range is " ++
    (show $ fst $ errorRange e) ++ " -> " ++ (show $ snd $ errorRange e) ++
    " inclusive."
  showUserError e =
    formatUserError
      "IndexOutOfBoundsError"
      "Index value of " ++ (show $ errorIndex e) ++ " is not in valid range [" ++ (show $ fst $ errorRange e) ++ ", " ++ (show $ snd $ errorRange e) ++ "]"

--------------------------------------------------------------------------------

data RandomError = RandomError { randomErrorMsg :: String }

instance BotError RandomError where
  showError e = "RandomError: " ++ (randomErrorMsg e)
  showUserError e = formatUserError "RandomError" (randomErrorMsg e)
