-- |
-- Module      : Sahasrara.Utility.Permission
-- Description : A simple interface to allow plugins to handle permissions themselves
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This contains a simple interface for plugin authors to require a specific level of privilege.
module Sahasrara.Utility.Permission where

import Discord.Internal.Rest (Message)
import Sahasrara.Internal.Permission
import Sahasrara.Utility.Discord (sendMessage)
import Sahasrara.Utility.Types

-- | @requirePermission@ only runs the inputted effect if permissions are matched. Otherwise it returns an error.
requirePermission :: RequiredPermission -> Message -> EnvDatabaseDiscord s () -> EnvDatabaseDiscord s ()
requirePermission perm m a = do
  p <- getSenderPermission m
  if userHasPermission perm p
    then a
    else sendMessage m "Sorry, you don't have permission to do that."
