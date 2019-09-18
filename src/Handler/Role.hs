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
getRoleIdR :: RoleId -> Handler Value
getRoleIdR roleId = do
                    role <- runDB $ getJustEntity roleId
                    returnJson $ buildRoleResponse role

-- DELETE ROLE BY ID
deleteRoleIdR :: RoleId -> Handler Value
deleteRoleIdR roleId = do
                        _ <- runDB $ delete roleId
                        sendResponseStatus status200 ("DELETED" :: Text)

-- LIST ROLES ENDPOINT
getRoleR :: Handler Value
getRoleR = do
            roles <- runDB $ selectList ([] :: [Filter Role]) []
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
                         let roleKey = (toSqlKey $ fromIntegral $ R.getRoleId role)::RoleId
                         _ <- runDB $ update roleKey [  RoleName =. R.getRoleName role
                                                      , RoleActive =. R.getActive role
                                                     ]
                         return roleKey
                      else do
                            roleKey <- runDB $ insert $ fromRoleDT role
                            return roleKey
            response <- getRoleIdR roleId
            returnJson response

-- GET PRIVILEGES FOR ROLE
getRolePrivilegeR :: RoleId -> Handler Value
getRolePrivilegeR roleId = do
                            entityRolePrivileges <- runDB $ selectList ([RolePrivilegeRoleId ==. roleId] :: [Filter RolePrivilege]) []
                            let privilegeIds = P.map (\(Entity _ (RolePrivilege _ privilegeId)) -> privilegeId) entityRolePrivileges
                            entityPrivileges <- runDB $ selectList ([PrivilegeId <-. privilegeIds] :: [Filter Privilege]) []
                            returnJson entityPrivileges

-- ADD PRIVILEGES TO ROLE
postRolePrivilegeR :: RoleId -> Handler Value
postRolePrivilegeR roleId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [PrivilegeId]
                            entityRolePrivileges <- runDB $ selectList ([RolePrivilegeRoleId ==. roleId] :: [Filter RolePrivilege]) []
                            let existingPrivilegeIds = P.map (\(Entity _ (RolePrivilege _ privilegeId)) -> privilegeId) entityRolePrivileges
                            let removableIds = S.toList $ S.difference (S.fromList existingPrivilegeIds) (S.fromList requestPrivilegeIds)
                            let newIds = S.toList $ S.difference (S.fromList requestPrivilegeIds) (S.fromList existingPrivilegeIds)
                            _ <- runDB $ deleteWhere  ([RolePrivilegePrivilegeId <-. removableIds] :: [Filter RolePrivilege])
                            _ <- runDB $ insertMany $ P.map (\privilegeId -> (RolePrivilege roleId privilegeId)) newIds
                            privileges <- getRolePrivilegeR roleId
                            returnJson privileges

-- DELETE PRIVILEGES FOR ROLE
deleteRolePrivilegeR :: RoleId -> Handler Value
deleteRolePrivilegeR roleId = do
                            requestPrivilegeIds <- requireCheckJsonBody :: Handler [PrivilegeId]
                            _ <- runDB $ deleteWhere  ([RolePrivilegePrivilegeId <-. requestPrivilegeIds] :: [Filter RolePrivilege])
                            privileges <- getRolePrivilegeR roleId
                            returnJson privileges

buildRoleResponse :: Entity Role -> R.Role
buildRoleResponse (Entity roleId role) = R.Role {  roleId = fromIntegral $ fromSqlKey roleId
                                                 , roleName = a
                                                 , description = b
                                                 , active = c
                                                 , privileges = []
                                                }
                                  where
                                    Role a b c = role


fromRoleDT :: R.Role -> Role
fromRoleDT R.Role {..} = Role roleName description active