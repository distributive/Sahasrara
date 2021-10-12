module Tablebot.Handler.Permission where

import Tablebot.Handler.Permission
    
requirePermission :: RequiredPermission -> Message -> DatabaseDiscord () -> DatabaseDiscord ()
requirePermission p m a = enforcePermission p (a m) m
