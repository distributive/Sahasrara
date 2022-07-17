-- |
-- Module      : Sahasrara.Internal.Permission
-- Description : Some internal code for handling permissions
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- This contains some functions to extract and handle privileged commands
module Sahasrara.Internal.Permission where

import Control.Monad.IO.Class (liftIO)
import Discord.Types (GuildMember, RoleId, memberRoles)
import Sahasrara.Utility.SmartParser (Context (contextMember))
import Sahasrara.Utility.Types
import Sahasrara.Utility.Utils (isDebug)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data KnownRoles = KnownRoles
  { krModerator :: Maybe RoleId,
    krSuperuser :: Maybe RoleId
  }
  deriving (Show)

userHasPermission :: RequiredPermission -> UserPermission -> Bool
userHasPermission _ (UserPerm _ True) = True -- Superuser always has perm
userHasPermission None _ = True
userHasPermission Moderator (UserPerm moderator _) = moderator
userHasPermission _ _ = False

getKnownRoles :: IO KnownRoles
getKnownRoles = do
  moderator <- lookupEnv "MODERATOR_GROUP"
  superuser <- lookupEnv "SUPERUSER_GROUP"
  return $ KnownRoles (maybeRead moderator) (maybeRead superuser)
  where
    maybeRead (Just a) = readMaybe a
    maybeRead Nothing = Nothing

getMemberGroups :: Maybe GuildMember -> [RoleId]
getMemberGroups (Just gm) = memberRoles gm
getMemberGroups Nothing = []

permsFromGroups :: Bool -> KnownRoles -> [RoleId] -> UserPermission
permsFromGroups debug krls gps =
  UserPerm
    (debug || krModerator krls `elemish` gps)
    (debug || krSuperuser krls `elemish` gps)
  where
    elemish (Just a) b = a `elem` b
    elemish Nothing _ = False

getSenderPermission :: Context m => m -> EnvDatabaseDiscord s UserPermission
getSenderPermission m = do
  let member = contextMember m
  knownroles <- liftIO getKnownRoles
  debug <- liftIO isDebug
  return $ permsFromGroups debug knownroles $ getMemberGroups member
