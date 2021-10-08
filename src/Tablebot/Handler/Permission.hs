module Tablebot.Handler.Permission (userHasPermission, enforcePermission) where

import Tablebot.Plugin.Types
import Tablebot.Plugin.Discord (getMessageMember, sendMessageVoid)
import Discord.Types (Message, GuildMember, RoleId, memberRoles)
import Discord (RestCallErrorCode)
import Data.Maybe (fromJust, isJust)
import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO)

data KnownRoles = KnownRoles {
    krExec :: Maybe RoleId, 
    krModerator :: Maybe RoleId, 
    krSuperuser :: Maybe RoleId
  }

userHasPermission :: RequiredPermission -> UserPermission -> Bool
userHasPermission _ (UserPerm _ _ True) = True -- Superuser always has perm
userHasPermission None _ = True
userHasPermission Any (UserPerm exec moderator _) = exec || moderator
userHasPermission Exec (UserPerm exec _ _) = exec
userHasPermission Moderator (UserPerm _ moderator _) = moderator
userHasPermission _ _ = False

enforcePermission :: RequiredPermission -> (Message -> DatabaseDiscord ()) -> Message -> DatabaseDiscord ()
enforcePermission perm a m = do
  p <- getSenderPermission m
  if userHasPermission perm p then
    a m
  else
    sendMessageVoid m "Sorry, you don't have permission to do that."

getKnownRoles :: IO (KnownRoles)
getKnownRoles = do
  exec <- lookupEnv "EXEC_GROUP"
  moderator <- lookupEnv "MODERATOR_GROUP"
  superuser <- lookupEnv "SUPERUSER_GROUP"
  return $ KnownRoles (maybeRead exec) (maybeRead moderator) (maybeRead superuser)
  where
    maybeRead (Just a) = read a
    maybeRead Nothing = Nothing

getMemberGroups :: Maybe GuildMember -> [RoleId]
getMemberGroups (Just gm) = memberRoles gm
getMemberGroups Nothing = []

permsFromGroups :: KnownRoles -> [RoleId] -> UserPermission
permsFromGroups krls gps = UserPerm
      (krExec krls `elemish` gps)
      (krModerator krls `elemish` gps)
      (krSuperuser krls `elemish` gps)
    where
      elemish (Just a) b = a `elem` b
      elemish Nothing _ = True

getSenderPermission :: Message -> DatabaseDiscord (UserPermission)
getSenderPermission m = do
  member <- getMessageMember m
  knownroles <- liftIO getKnownRoles
  return $ permsFromGroups knownroles $ getMemberGroups member