{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards #-}

module Business.UserBizFunc where

import qualified Prelude as P
import qualified Data.Set as S
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Crypto.BCrypt
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified DataTransfer.Person as DT
import Import
import qualified DataTransfer.User as U


-- GET USER BY ID
getUserByIdBizFunc :: User_Id -> Handler U.User
getUserByIdBizFunc userId = do
                              user <- runDB $ getJustEntity userId
                              return $ buildUserResponse user

-- DELETE USER BY ID
deleteUserBizFunc :: User_Id -> Handler Bool
deleteUserBizFunc userId = do
                            _ <- runDB $ delete userId
                            return True

-- LIST USERS
listUsersBizFunc :: Handler [U.User]
listUsersBizFunc = do
                    users <- runDB $ selectList [] [Asc User_Id]
                    return $ P.map buildUserResponse users

-- CREATE OR UPDATE USER
createOrUpdateUserBizFunc :: U.User -> Handler U.User
createOrUpdateUserBizFunc user = do
                                  userId <- if (U.getUserId user) > 0 then
                                              do
                                               let userKey = (toSqlKey $ fromIntegral $ U.getUserId user)::User_Id
                                               _ <- runDB $ update userKey [  User_Username =. U.getUsername user
                                                                            , User_Email =. U.getEmail user
                                                                            , User_Active =. U.getActive user
                                                                           ]
                                               return userKey
                                            else do
                                                  userEncrypeted <- liftIO $ encryptPassword $ fromUserDT user
                                                  userKey <- runDB $ insert userEncrypeted
                                                  return userKey
                                  response <- getUserByIdBizFunc userId
                                  return response

-- GET ROLES FOR USER
listUserRolesBizFunc :: User_Id -> Handler [Entity Role_]
listUserRolesBizFunc userId = do
                                entityUserRoles <- runDB $ selectList ([UserRole_UserId ==. userId] :: [Filter UserRole_]) []
                                let roleIds = P.map (\(Entity _ (UserRole_ _ roleId)) -> roleId) entityUserRoles
                                entityRoles <- runDB $ selectList ([Role_Id <-. roleIds] :: [Filter Role_]) []
                                return entityRoles

-- ADD ROLES TO USER
addUserRoleBizFunc :: User_Id -> [Role_Id] -> Handler [Entity Role_]
addUserRoleBizFunc userId requestRoleIds = do
                                            entityUserRoles <- runDB $ selectList ([UserRole_UserId ==. userId] :: [Filter UserRole_]) []
                                            let existingRoleIds = P.map (\(Entity _ (UserRole_ _ roleId)) -> roleId) entityUserRoles
                                            let removableIds = S.toList $ S.difference (S.fromList existingRoleIds) (S.fromList requestRoleIds)
                                            let newIds = S.toList $ S.difference (S.fromList requestRoleIds) (S.fromList existingRoleIds)
                                            _ <- runDB $ deleteWhere  ([UserRole_RoleId <-. removableIds] :: [Filter UserRole_])
                                            _ <- runDB $ insertMany $ P.map (\roleId -> (UserRole_ userId roleId)) newIds
                                            roles <- listUserRolesBizFunc userId
                                            return roles

-- DELETE ROLES FOR USER
deleteUserRoleBizFunc :: User_Id -> [Role_Id] -> Handler [Entity Role_]
deleteUserRoleBizFunc userId requestRoleIds = do
                                                _ <- runDB $ deleteWhere  ([UserRole_RoleId <-. requestRoleIds] :: [Filter UserRole_])
                                                roles <- listUserRolesBizFunc userId
                                                return roles

-- GET PRIVILEGES FOR USER
listUserPrivilegeBizFunc :: User_Id -> Handler [Entity Privilege_]
listUserPrivilegeBizFunc userId = do
                                    entityUserPrivileges <- runDB $ selectList ([UserPrivilege_UserId ==. userId] :: [Filter UserPrivilege_]) []
                                    let privilegeIds = P.map (\(Entity _ (UserPrivilege_ _ privilegeId)) -> privilegeId) entityUserPrivileges
                                    entityPrivileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
                                    return entityPrivileges

-- ADD PRIVILEGES TO USER
addUserPrivilegeBizFunc :: User_Id -> [Privilege_Id] -> Handler [Entity Privilege_]
addUserPrivilegeBizFunc userId requestPrivilegeIds = do
                                                      entityUserPrivileges <- runDB $ selectList ([UserPrivilege_UserId ==. userId] :: [Filter UserPrivilege_]) []
                                                      let existingPrivilegeIds = P.map (\(Entity _ (UserPrivilege_ _ privilegeId)) -> privilegeId) entityUserPrivileges
                                                      let removableIds = S.toList $ S.difference (S.fromList existingPrivilegeIds) (S.fromList requestPrivilegeIds)
                                                      let newIds = S.toList $ S.difference (S.fromList requestPrivilegeIds) (S.fromList existingPrivilegeIds)
                                                      _ <- runDB $ deleteWhere  ([UserPrivilege_PrivilegeId <-. removableIds] :: [Filter UserPrivilege_])
                                                      _ <- runDB $ insertMany $ P.map (\privilegeId -> (UserPrivilege_ userId privilegeId)) newIds
                                                      privileges <- listUserPrivilegeBizFunc userId
                                                      return privileges

-- DELETE PRIVILEGES FOR USER
deleteUserPrivilegeBizFunc :: User_Id -> [Privilege_Id] -> Handler [Entity Privilege_]
deleteUserPrivilegeBizFunc userId requestPrivilegeIds = do
                                                          _ <- runDB $ deleteWhere  ([UserPrivilege_PrivilegeId <-. requestPrivilegeIds] :: [Filter UserPrivilege_])
                                                          privileges <- listUserPrivilegeBizFunc userId
                                                          return privileges

-- User username password enabled ident
encryptPassword:: User_ -> IO User_
encryptPassword (User_ a b c d) = do
                                    p <- liftIO $ getPasswordIO c
                                    return (User_ a b p d)

getPasswordIO:: Text->IO Text
getPasswordIO password = do
                    p <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (B.pack (T.unpack password))
                    return ( case p of
                             Nothing -> ""
                             Just b -> T.pack $ B.unpack b
                            )


buildUserResponse :: Entity User_ -> U.User
buildUserResponse (Entity userId user) = U.User {  userId = fromIntegral $ fromSqlKey userId
                                                 , username = a
                                                 , email = b
                                                 , password = "######"
                                                 , active = d
                                                 , roles = []
                                                 , privileges = []
                                                }
                                  where
                                    User_ a b _ d = user


fromUserDT :: U.User -> User_
fromUserDT U.User {..} = User_ username email password active





-- postUserR :: Handler Value
-- postUserR = do
--                    user <- requireJsonBody :: Handler User
--                    user1 <- liftIO $ encryptPassword user
--                    newUser <- runDB $ insertEntity user1
--                    returnJson newUser
