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

module Graphql.Role (Roles, Role, resolveRole) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
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

data ListArgs = ListArgs { queryString :: Text, pageable :: Maybe Pageable } deriving (Generic)

-- DB ACTIONS
dbFetchRoleById:: Role_Id -> Handler Role
dbFetchRoleById roleId = do
                          role <- runDB $ getJustEntity roleId
                          return $ toRoleQL role

dbFetchPrivileges:: ListArgs -> Handler [Role]
dbFetchPrivileges ListArgs {..} = do
                                  roles <- runDB $ selectList [] [Asc Role_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                                  return $ P.map toRoleQL roles
                              where
                                (page, size) = case pageable of
                                                Just (Pageable x y) -> (x, y)
                                                Nothing -> (1, 10)

-- Query Resolvers
findByIdResolver :: FindByIdArgs -> Res e Handler Role
findByIdResolver FindByIdArgs { roleId } = lift $ dbFetchRoleById roleKey
                                              where
                                                roleKey = (toSqlKey $ fromIntegral $ roleId)::Role_Id

listResolver :: ListArgs -> Res e Handler [Role]
listResolver listArgs = lift $ dbFetchPrivileges listArgs

resolveRole :: Roles (Res () Handler)
resolveRole = Roles {  findById = findByIdResolver, list = listResolver }


-- CONVERTERS
--     Id sql=role_id
--     key Text
--     name Text
--     description Text Maybe
--     active Bool
--     createdDate UTCTime
--     modifiedDate UTCTime
toRoleQL :: Entity Role_ -> Role
toRoleQL (Entity roleId role) = Role { roleId = fromIntegral $ fromSqlKey roleId
                                          , key = role_Key
                                          , name = role_Name
                                          , description = role_Description
                                          , active = role_Active
                                          , createdDate = Just $ fromString $ show role_CreatedDate
                                          , modifiedDate = m
                                          , privileges = []
                                          }
                                where
                                  Role_ {..} = role
                                  m = case role_ModifiedDate of
                                        Just d -> Just $ fromString $ show d
                                        Nothing -> Nothing

fromPrivilegeQL :: Role -> UTCTime -> Maybe UTCTime -> Role_
fromPrivilegeQL (Role {..}) cd md = Role_ { role_Key = key
                                          , role_Name = name
                                          , role_Description = description
                                          , role_Active = active
                                          , role_CreatedDate = cd
                                          , role_ModifiedDate = md
                                          }
