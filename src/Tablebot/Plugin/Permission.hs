-- |
-- Module      : Tablebot.Plugin.Permission
-- Description : A simple interface to allow plugins to handle permissions themselves
-- Copyright   : (c) Anna Bruce 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This contains a simple interface for plugin authors to require a specific level of privilege.
module Tablebot.Plugin.Permission where

import Discord.Internal.Rest (Message)
import Tablebot.Handler.Permission
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.Types

-- | @requirePermission@ only runs the inputted effect if permissions are matched. Otherwise it returns an error.
requirePermission :: RequiredPermission -> Message -> EnvDatabaseDiscord s () -> EnvDatabaseDiscord s ()
requirePermission perm m a = do
  p <- getSenderPermission m
  if userHasPermission perm p
    then a
    else sendMessage m "Sorry, you don't have permission to do that."
