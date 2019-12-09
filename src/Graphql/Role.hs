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

module Graphql.Role (Roles, Role, RoleArg, resolveRole, resolveSaveRole) where

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
                 , createdDate :: Maybe Text
                 , modifiedDate :: Maybe Text
                 , privileges :: [Privilege]
                 } deriving (Generic, GQLType)

data Roles m = Roles { findById :: FindByIdArgs -> m Role
                     , list :: ListArgs -> m [Role]
                     } deriving (Generic, GQLType)

data FindByIdArgs = FindByIdArgs { roleId :: Int } deriving (Generic)

-- data ListArgs = ListArgs { queryString :: Text, pageable :: Maybe Pageable } deriving (Generic)

data RoleArg = RoleArg { roleId :: Int
                       , key :: Text
                       , name :: Text
                       , description :: Maybe Text
                       , active :: Bool
                       , privileges :: [Int]
                       } deriving (Generic, GQLType)

-- DB ACTIONS
dbFetchRoleById:: Role_Id -> Handler Role
dbFetchRoleById roleId = do
                          role <- runDB $ getJustEntity roleId
                          privileges <- dbFetchPrivileges roleId
                          return $ toRoleQL role privileges

dbFetchRoles:: ListArgs -> Handler [Role]
dbFetchRoles ListArgs {..} = do
                              roles <- runDB $ selectList [] [Asc Role_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                              return $ P.map (\r -> toRoleQL r []) roles
                         where
                          (page, size) = case pageable of
                                          Just (Pageable x y) -> (x, y)
                                          Nothing -> (1, 10)

dbFetchPrivileges:: Role_Id -> Handler [Privilege]
dbFetchPrivileges roleId = do
                            rolePrivileges <- runDB $ selectList ([RolePrivilege_RoleId ==. roleId] :: [Filter RolePrivilege_]) []
                            let privilegeIds = P.map (\(Entity _ (RolePrivilege_ _ privilegeId)) -> privilegeId) rolePrivileges
                            privileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
                            return $ P.map toPrivilegeQL privileges

-- Query Resolvers
findByIdResolver :: FindByIdArgs -> Res e Handler Role
findByIdResolver FindByIdArgs { roleId } = lift $ dbFetchRoleById roleKey
                                              where
                                                roleKey = (toSqlKey $ fromIntegral $ roleId)::Role_Id

listResolver :: ListArgs -> Res e Handler [Role]
listResolver listArgs = lift $ dbFetchRoles listArgs

resolveRole :: Roles (Res () Handler)
resolveRole = Roles {  findById = findByIdResolver, list = listResolver }

-- Mutation Resolvers
resolveSaveRole :: RoleArg -> MutRes e Handler Role
resolveSaveRole arg = lift $ createOrUpdateRole arg

createOrUpdateRole :: RoleArg -> Handler Role
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
                                            roleKey <- runDB $ insert $ fromRoleArg role now Nothing
                                            return roleKey
                            let requestPrivilegeIds = P.map (\ x -> (toSqlKey $ fromIntegral $ x)::Privilege_Id) privileges
                            entityRolePrivileges <- runDB $ selectList ([RolePrivilege_RoleId ==. roleEntityId] :: [Filter RolePrivilege_]) []
                            let existingPrivilegeIds = P.map (\(Entity _ (RolePrivilege_ _ privilegeId)) -> privilegeId) entityRolePrivileges
                            let removableIds = S.toList $ S.difference (S.fromList existingPrivilegeIds) (S.fromList requestPrivilegeIds)
                            let newIds = S.toList $ S.difference (S.fromList requestPrivilegeIds) (S.fromList existingPrivilegeIds)
                            _ <- runDB $ deleteWhere  ([RolePrivilege_PrivilegeId <-. removableIds] :: [Filter RolePrivilege_])
                            _ <- runDB $ insertMany $ P.map (\privilegeId -> (RolePrivilege_ roleEntityId privilegeId)) newIds
                            response <- dbFetchRoleById roleEntityId
                            return response

-- CONVERTERS
--     Id sql=role_id
--     key Text
--     name Text
--     description Text Maybe
--     active Bool
--     createdDate UTCTime
--     modifiedDate UTCTime
toRoleQL :: Entity Role_ -> [Privilege] -> Role
toRoleQL (Entity roleId role) privileges = Role { roleId = fromIntegral $ fromSqlKey roleId
                                                , key = role_Key
                                                , name = role_Name
                                                , description = role_Description
                                                , active = role_Active
                                                , createdDate = Just $ fromString $ show role_CreatedDate
                                                , modifiedDate = m
                                                , privileges = privileges
                                                }
                                        where
                                          Role_ {..} = role
                                          m = case role_ModifiedDate of
                                                Just d -> Just $ fromString $ show d
                                                Nothing -> Nothing

fromRoleQL :: Role -> UTCTime -> Maybe UTCTime -> Role_
fromRoleQL (Role {..}) cd md = Role_ { role_Key = key
                                          , role_Name = name
                                          , role_Description = description
                                          , role_Active = active
                                          , role_CreatedDate = cd
                                          , role_ModifiedDate = md
                                          }

fromRoleArg :: RoleArg -> UTCTime -> Maybe UTCTime -> Role_
fromRoleArg (RoleArg {..}) cd md = Role_ { role_Key = key
                                         , role_Name = name
                                         , role_Description = description
                                         , role_Active = active
                                         , role_CreatedDate = cd
                                         , role_ModifiedDate = md
                                         }
