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

module Handler.Role where

import qualified Prelude as P
import qualified Data.Set as S
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified DataTransfer.Role as R
import Import


-- GET ROLE BY ID
getRoleIdR :: Role_Id -> Handler Value
getRoleIdR roleId = do
                    role <- runDB $ getJustEntity roleId
                    returnJson $ buildRoleResponse role

-- DELETE ROLE BY ID
deleteRoleIdR :: Role_Id -> Handler Value
deleteRoleIdR roleId = do
                        _ <- runDB $ delete roleId
                        sendResponseStatus status200 ("DELETED" :: Text)

-- LIST ROLES ENDPOINT
getRoleR :: Handler Value
getRoleR = do
            roles <- runDB $ selectList ([] :: [Filter Role_]) []
            returnJson $ P.map buildRoleResponse roles

-- CREATE ROLE ENDPOINT
postRoleR :: Handler Value
postRoleR = putRoleR

-- UPDATE ROLE ENDPOINT
putRoleR :: Handler Value
putRoleR = do
            role <- requireCheckJsonBody :: Handler R.Role
            roleId <- if (R.getRoleId role) > 0 then
                        do
                         let roleKey = (toSqlKey $ fromIntegral $ R.getRoleId role)::Role_Id
                         _ <- runDB $ update roleKey [  Role_Name =. R.getRoleName role
                                                      , Role_Active =. R.getActive role
                                                     ]
                         return roleKey
                      else do
                            roleKey <- runDB $ insert $ fromRoleDT role
                            return roleKey
            response <- getRoleIdR roleId
            returnJson response

-- GET PRIVILEGES FOR ROLE
getRolePrivilegeR :: Role_Id -> Handler Value
getRolePrivilegeR roleId = do
                            entityRolePrivileges <- runDB $ selectList ([RolePrivilege_RoleId ==. roleId] :: [Filter RolePrivilege_]) []
                            let privilegeIds = P.map (\(Entity _ (RolePrivilege_ _ privilegeId)) -> privilegeId) entityRolePrivileges
                            entityPrivileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
                            returnJson entityPrivileges

-- ADD PRIVILEGES TO ROLE
postRolePrivilegeR :: Role_Id -> Handler Value
postRolePrivilegeR roleId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [Privilege_Id]
                            entityRolePrivileges <- runDB $ selectList ([RolePrivilege_RoleId ==. roleId] :: [Filter RolePrivilege_]) []
                            let existingPrivilegeIds = P.map (\(Entity _ (RolePrivilege_ _ privilegeId)) -> privilegeId) entityRolePrivileges
                            let removableIds = S.toList $ S.difference (S.fromList existingPrivilegeIds) (S.fromList requestPrivilegeIds)
                            let newIds = S.toList $ S.difference (S.fromList requestPrivilegeIds) (S.fromList existingPrivilegeIds)
                            _ <- runDB $ deleteWhere  ([RolePrivilege_PrivilegeId <-. removableIds] :: [Filter RolePrivilege_])
                            _ <- runDB $ insertMany $ P.map (\privilegeId -> (RolePrivilege_ roleId privilegeId)) newIds
                            privileges <- getRolePrivilegeR roleId
                            returnJson privileges

-- DELETE PRIVILEGES FOR ROLE
deleteRolePrivilegeR :: Role_Id -> Handler Value
deleteRolePrivilegeR roleId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [Privilege_Id]
                            _ <- runDB $ deleteWhere  ([RolePrivilege_PrivilegeId <-. requestPrivilegeIds] :: [Filter RolePrivilege_])
                            privileges <- getRolePrivilegeR roleId
                            returnJson privileges

buildRoleResponse :: Entity Role_ -> R.Role
buildRoleResponse (Entity roleId role) = R.Role {  roleId = fromIntegral $ fromSqlKey roleId
                                                 , roleName = a
                                                 , description = b
                                                 , active = c
                                                 , privileges = []
                                                }
                                  where
                                    Role_ a b c = role


fromRoleDT :: R.Role -> Role_
fromRoleDT R.Role {..} = Role_ roleName description active
