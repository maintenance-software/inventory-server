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

import Import
import qualified DataTransfer.User as U
import Business.UserBizFunc (   getUserByIdBizFunc
                              , deleteUserBizFunc
                              , listUsersBizFunc
                              , createOrUpdateUserBizFunc
                              , listUserRolesBizFunc
                              , addUserRoleBizFunc
                              , deleteUserRoleBizFunc
                              , listUserPrivilegeBizFunc
                              , deleteUserPrivilegeBizFunc
                            )

-- GET USER BY ID
getUserIdR :: User_Id -> Handler Value
getUserIdR userId = do
                user <- getUserByIdBizFunc userId
                returnJson user

-- DELETE USER BY ID
deleteUserIdR :: User_Id -> Handler Value
deleteUserIdR userId = do
                        _ <- deleteUserBizFunc userId
                        sendResponseStatus status200 ("DELETED" :: Text)

-- LIST USERS ENDPOINT
getUserR :: Handler Value
getUserR = do
            users <- listUsersBizFunc
            returnJson users

-- CREATE USER ENDPOINT
postUserR :: Handler Value
postUserR = putUserR

-- UPDATE USER ENDPOINT
putUserR :: Handler Value
putUserR = do
            user <- requireCheckJsonBody :: Handler U.User
            response <- createOrUpdateUserBizFunc user
            returnJson response

-- GET ROLES FOR USER
getUserRoleR :: User_Id -> Handler Value
getUserRoleR userId = do
                        roles <- listUserRolesBizFunc userId
                        returnJson roles

-- ADD ROLES TO USER
postUserRoleR :: User_Id -> Handler Value
postUserRoleR userId = do
                        requestRoleIds <- requireCheckJsonBody :: Handler [Role_Id]
                        roles <- addUserRoleBizFunc userId requestRoleIds
                        returnJson roles

-- DELETE ROLES FOR USER
deleteUserRoleR :: User_Id -> Handler Value
deleteUserRoleR userId = do
                        requestRoleIds <- requireCheckJsonBody :: Handler [Role_Id]
                        roles <- deleteUserRoleBizFunc userId requestRoleIds
                        returnJson roles

-- GET PRIVILEGES FOR USER
getUserPrivilegeR :: User_Id -> Handler Value
getUserPrivilegeR userId = do
                            privileges <- listUserPrivilegeBizFunc userId
                            returnJson privileges

-- ADD PRIVILEGES TO USER
postUserPrivilegeR :: User_Id -> Handler Value
postUserPrivilegeR userId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [Privilege_Id]
                            privileges <- deleteUserPrivilegeBizFunc userId requestPrivilegeIds
                            returnJson privileges

-- DELETE PRIVILEGES FOR USER
deleteUserPrivilegeR :: User_Id -> Handler Value
deleteUserPrivilegeR userId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [Privilege_Id]
                            privileges <- deleteUserPrivilegeBizFunc userId requestPrivilegeIds
                            returnJson privileges

-- postUserR :: Handler Value
-- postUserR = do
--                    user <- requireJsonBody :: Handler User
--                    user1 <- liftIO $ encryptPassword user
--                    newUser <- runDB $ insertEntity user1
--                    returnJson newUser
