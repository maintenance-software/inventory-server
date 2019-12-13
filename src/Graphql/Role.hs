{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Graphql.Role (Roles, Role, RoleArg, resolveRole, RoleMut, resolveSaveRole) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Data.Time
import Graphql.Privilege

data Role = Role { roleId :: Int
                 , key :: Text
                 , name :: Text
                 , description :: Maybe Text
                 , active :: Bool
                 , createdDate :: Text
                 , modifiedDate :: Maybe Text
                 , privileges :: DummyArg -> Res () Handler [Privilege]
                 } deriving (Generic, GQLType)

data Roles m = Roles { role :: GetEntityByIdArg -> m Role
                     , list :: ListArgs -> m [Role]
                     } deriving (Generic, GQLType)

data RoleArg = RoleArg { roleId :: Int
                       , key :: Text
                       , name :: Text
                       , description :: Maybe Text
                       , active :: Bool
                       } deriving (Generic, GQLType)

-- Query Resolvers
findByIdResolver :: GetEntityByIdArg -> Res e Handler Role
findByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let roleId = (toSqlKey $ fromIntegral $ entityId)::Role_Id
                                              role <- runDB $ getJustEntity roleId
                                              return $ toRoleQL role

listResolver :: ListArgs -> Res e Handler [Role]
listResolver ListArgs {..} = lift $ do
                        roles <- runDB $ selectList [] [Asc Role_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                        return $ P.map (\r -> toRoleQL r) roles
                         where
                          (page, size) = case pageable of
                                          Just (Pageable x y) -> (x, y)
                                          Nothing -> (1, 10)

resolveRole :: Roles (Res () Handler)
resolveRole = Roles {  role = findByIdResolver, list = listResolver }

resolvePrivileges :: Role_Id -> DummyArg -> Res e Handler [Privilege]
resolvePrivileges roleId arg = lift $ do
                                      rolePrivileges <- runDB $ selectList ([RolePrivilege_RoleId ==. roleId] :: [Filter RolePrivilege_]) []
                                      let privilegeIds = P.map (\(Entity _ (RolePrivilege_ _ privilegeId)) -> privilegeId) rolePrivileges
                                      privileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
                                      return $ P.map toPrivilegeQL privileges

toRoleQL :: Entity Role_ -> Role
toRoleQL (Entity roleId role) = Role { roleId = fromIntegral $ fromSqlKey roleId
                                     , key = role_Key
                                     , name = role_Name
                                     , description = role_Description
                                     , active = role_Active
                                     , createdDate = fromString $ show role_CreatedDate
                                     , modifiedDate = m
                                     , privileges = resolvePrivileges roleId
                                     }
                            where
                              Role_ {..} = role
                              m = case role_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing

data RoleMut = RoleMut { roleId :: Int
                       , key :: Text
                       , name :: Text
                       , description :: Maybe Text
                       , active :: Bool
                       , createdDate :: Text
                       , modifiedDate :: Maybe Text
                       , privileges :: EntityIdsArg -> MutRes () Handler [Privilege]
                       } deriving (Generic, GQLType)

-- Mutation Resolvers
resolveSaveRole :: RoleArg -> MutRes e Handler RoleMut
resolveSaveRole arg = lift $ do
                              roleId <- createOrUpdateRole arg
                              role <- runDB $ getJustEntity roleId
                              let Entity _ Role_ {..} = role
                              let md = case role_ModifiedDate of
                                        Just d -> Just $ fromString $ show d
                                        Nothing -> Nothing
                              return RoleMut { roleId = fromIntegral $ fromSqlKey roleId
                                             , key = role_Key
                                             , name = role_Name
                                             , description = role_Description
                                             , active = role_Active
                                             , createdDate = fromString $ show role_CreatedDate
                                             , modifiedDate = md
                                             , privileges = resolveSaveRolePrivileges roleId
                                             }

resolveSaveRolePrivileges :: Role_Id -> EntityIdsArg -> MutRes e Handler [Privilege]
resolveSaveRolePrivileges roleId EntityIdsArg {..} = lift $ do
                                          () <- createOrUpdateRolePrivilege roleId entityIds
                                          rolePrivileges <- runDB $ selectList ([RolePrivilege_RoleId ==. roleId] :: [Filter RolePrivilege_]) []
                                          let privilegeIds = P.map (\(Entity _ (RolePrivilege_ _ privilegeId)) -> privilegeId) rolePrivileges
                                          privileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
                                          return $ P.map toPrivilegeQL privileges

createOrUpdateRole :: RoleArg -> Handler Role_Id
createOrUpdateRole role = do
                            let RoleArg {..} = role
                            now <- liftIO getCurrentTime
                            roleEntityId <- if roleId > 0 then
                                        do
                                         let roleKey = (toSqlKey $ fromIntegral $ roleId)::Role_Id
                                         _ <- runDB $ update roleKey [ Role_Key =. key
                                                                     , Role_Name =. name
                                                                     , Role_Description =. description
                                                                     , Role_Active =. active
                                                                     , Role_ModifiedDate =. Just now
                                                                     ]
                                         return roleKey
                                      else do
                                            roleKey <- runDB $ insert $ fromRoleQL role now Nothing
                                            return roleKey
                            return roleEntityId

createOrUpdateRolePrivilege :: Role_Id -> [Int] -> Handler ()
createOrUpdateRolePrivilege roleId privileges = do
                            let entityPrivilegeIds = P.map (\ x -> (toSqlKey $ fromIntegral $ x)::Privilege_Id) privileges
                            entityRolePrivileges <- runDB $ selectList ([RolePrivilege_RoleId ==. roleId] :: [Filter RolePrivilege_]) []
                            let existingPrivilegeIds = P.map (\(Entity _ (RolePrivilege_ _ privilegeId)) -> privilegeId) entityRolePrivileges
                            let removableIds = S.toList $ S.difference (S.fromList existingPrivilegeIds) (S.fromList entityPrivilegeIds)
                            let newIds = S.toList $ S.difference (S.fromList entityPrivilegeIds) (S.fromList existingPrivilegeIds)
                            _ <- runDB $ deleteWhere  ([RolePrivilege_PrivilegeId <-. removableIds] :: [Filter RolePrivilege_])
                            _ <- runDB $ insertMany $ P.map (\privilegeId -> (RolePrivilege_ roleId privilegeId)) newIds
                            return ()

fromRoleQL :: RoleArg -> UTCTime -> Maybe UTCTime -> Role_
fromRoleQL (RoleArg {..}) cd md = Role_ { role_Key = key
                                        , role_Name = name
                                        , role_Description = description
                                        , role_Active = active
                                        , role_CreatedDate = cd
                                        , role_ModifiedDate = md
                                        }

{-
query {
  roles {
     role(entityId: 1) {
      roleId
      key
      description
      active
      createdDate
      modifiedDate
    }
    list(queryString: "") {
      roleId
      key
      description
      active
      createdDate
      modifiedDate
    }
  }
}

mutation {
  saveRole(roleId:10, key: "test12", name: "sloss", description: "option" active: true) {
    roleId
    key
    description
    active
    createdDate
    modifiedDate
    privileges(entityIds: [16]) {
      privilegeId
      key
      description
      active
      createdDate
      modifiedDate
    }
  }
}

-}
