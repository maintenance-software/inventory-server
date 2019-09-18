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

module Handler.User where

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
getUserIdR :: UserId -> Handler Value
getUserIdR userId = do
                user <- runDB $ getJustEntity userId
                returnJson $ buildUserResponse user

-- DELETE USER BY ID
deleteUserIdR :: UserId -> Handler Value
deleteUserIdR userId = do
                        _ <- runDB $ delete userId
                        sendResponseStatus status200 ("DELETED" :: Text)

-- LIST USERS ENDPOINT
getUserR :: Handler Value
getUserR = do
            users <- runDB $ selectList [] [Asc UserId]
            returnJson $ P.map buildUserResponse users

-- CREATE USER ENDPOINT
postUserR :: Handler Value
postUserR = putUserR

-- UPDATE USER ENDPOINT
putUserR :: Handler Value
putUserR = do
            user <- requireCheckJsonBody :: Handler U.User
            userId <- if (U.getUserId user) > 0 then
                        do
                         let userKey = (toSqlKey $ fromIntegral $ U.getUserId user)::UserId
                         _ <- runDB $ update userKey [  UserUsername =. U.getUsername user
                                                      , UserEmail =. U.getEmail user
                                                      , UserActive =. U.getActive user
                                                     ]
                         return userKey
                      else do
                            userEncrypeted <- liftIO $ encryptPassword $ fromUserDT user
                            userKey <- runDB $ insert userEncrypeted
                            return userKey
            response <- getUserIdR userId
            returnJson response

-- GET ROLES FOR USER
getUserRoleR :: UserId -> Handler Value
getUserRoleR userId = do
                        entityUserRoles <- runDB $ selectList ([UserRoleUserId ==. userId] :: [Filter UserRole]) []
                        let roleIds = P.map (\(Entity _ (UserRole _ roleId)) -> roleId) entityUserRoles
                        entityRoles <- runDB $ selectList ([RoleId <-. roleIds] :: [Filter Role]) []
                        returnJson entityRoles

-- ADD ROLES TO USER
postUserRoleR :: UserId -> Handler Value
postUserRoleR userId = do
                        requestRoleIds <- requireCheckJsonBody :: Handler [RoleId]
                        entityUserRoles <- runDB $ selectList ([UserRoleUserId ==. userId] :: [Filter UserRole]) []
                        let existingRoleIds = P.map (\(Entity _ (UserRole _ roleId)) -> roleId) entityUserRoles
                        let removableIds = S.toList $ S.difference (S.fromList existingRoleIds) (S.fromList requestRoleIds)
                        let newIds = S.toList $ S.difference (S.fromList requestRoleIds) (S.fromList existingRoleIds)
                        _ <- runDB $ deleteWhere  ([UserRoleRoleId <-. removableIds] :: [Filter UserRole])
                        _ <- runDB $ insertMany $ P.map (\roleId -> (UserRole userId roleId)) newIds
                        roles <- getUserRoleR userId
                        returnJson roles

-- DELETE ROLES FOR USER
deleteUserRoleR :: UserId -> Handler Value
deleteUserRoleR userId = do
                        requestRoleIds <- requireCheckJsonBody :: Handler [RoleId]
                        _ <- runDB $ deleteWhere  ([UserRoleRoleId <-. requestRoleIds] :: [Filter UserRole])
                        roles <- getUserRoleR userId
                        returnJson roles

-- GET PRIVILEGES FOR USER
getUserPrivilegeR :: UserId -> Handler Value
getUserPrivilegeR userId = do
                            entityUserPrivileges <- runDB $ selectList ([UserPrivilegeUserId ==. userId] :: [Filter UserPrivilege]) []
                            let privilegeIds = P.map (\(Entity _ (UserPrivilege _ privilegeId)) -> privilegeId) entityUserPrivileges
                            entityPrivileges <- runDB $ selectList ([PrivilegeId <-. privilegeIds] :: [Filter Privilege]) []
                            returnJson entityPrivileges

-- ADD PRIVILEGES TO USER
postUserPrivilegeR :: UserId -> Handler Value
postUserPrivilegeR userId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [PrivilegeId]
                            entityUserPrivileges <- runDB $ selectList ([UserPrivilegeUserId ==. userId] :: [Filter UserPrivilege]) []
                            let existingPrivilegeIds = P.map (\(Entity _ (UserPrivilege _ privilegeId)) -> privilegeId) entityUserPrivileges
                            let removableIds = S.toList $ S.difference (S.fromList existingPrivilegeIds) (S.fromList requestPrivilegeIds)
                            let newIds = S.toList $ S.difference (S.fromList requestPrivilegeIds) (S.fromList existingPrivilegeIds)
                            _ <- runDB $ deleteWhere  ([UserPrivilegePrivilegeId <-. removableIds] :: [Filter UserPrivilege])
                            _ <- runDB $ insertMany $ P.map (\privilegeId -> (UserPrivilege userId privilegeId)) newIds
                            privileges <- getUserPrivilegeR userId
                            returnJson privileges

-- DELETE PRIVILEGES FOR USER
deleteUserPrivilegeR :: UserId -> Handler Value
deleteUserPrivilegeR userId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [PrivilegeId]
                            _ <- runDB $ deleteWhere  ([UserPrivilegePrivilegeId <-. requestPrivilegeIds] :: [Filter UserPrivilege])
                            privileges <- getUserPrivilegeR userId
                            returnJson privileges

-- User username password enabled ident
encryptPassword:: User -> IO User
encryptPassword (User a b c d) = do
                                    p <- liftIO $ getPasswordIO c
                                    return (User a b p d)

getPasswordIO:: Text->IO Text
getPasswordIO password = do
                    p <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (B.pack (T.unpack password))
                    return ( case p of
                             Nothing -> ""
                             Just b -> T.pack $ B.unpack b
                            )


buildUserResponse :: Entity User -> U.User
buildUserResponse (Entity userId user) = U.User {  userId = fromIntegral $ fromSqlKey userId
                                                 , username = a
                                                 , email = b
                                                 , password = "######"
                                                 , active = d
                                                 , roles = []
                                                 , privileges = []
                                                }
                                  where
                                    User a b _ d = user


fromUserDT :: U.User -> User
fromUserDT U.User {..} = User username email password active





-- postUserR :: Handler Value
-- postUserR = do
--                    user <- requireJsonBody :: Handler User
--                    user1 <- liftIO $ encryptPassword user
--                    newUser <- runDB $ insertEntity user1
--                    returnJson newUser