-- |
-- Module      : Tablebot.Internal.Permission
-- Description : Some internal code for handling permissions
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This contains some functions to extract and handle privileged commands
module Tablebot.Internal.Permission where

import Control.Monad.IO.Class (liftIO)
import Discord.Types (GuildMember, Message, RoleId, memberRoles)
import System.Environment (lookupEnv)
import Tablebot.Utility.Discord (getMessageMember)
import Tablebot.Utility.Types
import Tablebot.Utility.Utils (isDebug)
import Text.Read (readMaybe)

data KnownRoles = KnownRoles
  { krExec :: Maybe RoleId,
    krModerator :: Maybe RoleId,
    krSuperuser :: Maybe RoleId
  }
  deriving (Show)

userHasPermission :: RequiredPermission -> UserPermission -> Bool
userHasPermission _ (UserPerm _ _ True) = True -- Superuser always has perm
userHasPermission None _ = True
userHasPermission Any (UserPerm exec moderator _) = exec || moderator
userHasPermission Exec (UserPerm exec _ _) = exec
userHasPermission Moderator (UserPerm _ moderator _) = moderator
userHasPermission Both (UserPerm exec moderator _) = exec && moderator
userHasPermission _ _ = False

getKnownRoles :: IO KnownRoles
getKnownRoles = do
  exec <- lookupEnv "EXEC_GROUP"
  moderator <- lookupEnv "MODERATOR_GROUP"
  superuser <- lookupEnv "SUPERUSER_GROUP"
  return $ KnownRoles (maybeRead exec) (maybeRead moderator) (maybeRead superuser)
  where
    maybeRead (Just a) = readMaybe a
    maybeRead Nothing = Nothing

getMemberGroups :: Maybe GuildMember -> [RoleId]
getMemberGroups (Just gm) = memberRoles gm
getMemberGroups Nothing = []

permsFromGroups :: Bool -> KnownRoles -> [RoleId] -> UserPermission
permsFromGroups debug krls gps =
  UserPerm
    (debug || krExec krls `elemish` gps)
    (debug || krModerator krls `elemish` gps)
    (debug || krSuperuser krls `elemish` gps)
  where
    elemish (Just a) b = a `elem` b
    elemish Nothing _ = False

getSenderPermission :: Message -> EnvDatabaseDiscord s UserPermission
getSenderPermission m = do
  member <- getMessageMember m
  knownroles <- liftIO getKnownRoles
  debug <- liftIO isDebug
  return $ permsFromGroups debug knownroles $ getMemberGroups member
