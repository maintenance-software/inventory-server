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

module Graphql.Privilege (Privileges, Privilege, PrivilegeArg, resolvePrivilege, resolveSavePrivilege, toPrivilegeQL) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Graphql.Utils
import Data.Time

data Privilege = Privilege { privilegeId :: Int
                           , key :: Text
                           , name :: Text
                           , description :: Maybe Text
                           , active :: Bool
                           , createdDate :: Maybe Text
                           , modifiedDate :: Maybe Text
                           } deriving (Generic, GQLType)

data PrivilegeArg = PrivilegeArg { privilegeId :: Int
                                 , key :: Text
                                 , name :: Text
                                 , description :: Maybe Text
                                 , active :: Bool
                                 } deriving (Generic, GQLType)

data Privileges = Privileges { privilege :: GetEntityByIdArg -> Res () Handler Privilege
                             , list :: PageArg -> Res () Handler [Privilege]
                             } deriving (Generic, GQLType)

-- DB ACTIONS
dbFetchPrivilegeById:: Privilege_Id -> Handler Privilege
dbFetchPrivilegeById privilegeId = do
                                      privilege <- runDB $ getJustEntity privilegeId
                                      return $ toPrivilegeQL privilege

dbFetchPrivileges:: PageArg -> Handler [Privilege]
dbFetchPrivileges PageArg {..} = do
                                  privileges <- runDB $ selectList [] [Asc Privilege_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                                  return $ P.map toPrivilegeQL privileges
                              where
                                pageIndex' = case pageIndex of
                                              Just  x  -> x
                                              Nothing -> 0
                                pageSize' = case pageSize of
                                                Just y -> y
                                                Nothing -> 10

-- Query Resolvers
findByIdResolver :: GetEntityByIdArg -> Res e Handler Privilege
findByIdResolver GetEntityByIdArg {..} = lift $ dbFetchPrivilegeById privilegeId
                                              where
                                                privilegeId = (toSqlKey $ fromIntegral $ entityId)::Privilege_Id

listResolver :: PageArg -> Res e Handler [Privilege]
listResolver listArgs = lift $ dbFetchPrivileges listArgs

resolvePrivilege :: () -> Res e Handler Privileges
resolvePrivilege _ = pure Privileges {  privilege = findByIdResolver, list = listResolver }

-- Mutation Resolvers
resolveSavePrivilege :: PrivilegeArg -> MutRes e Handler Privilege
resolveSavePrivilege arg = lift $ createOrUpdatePrivilege arg

createOrUpdatePrivilege :: PrivilegeArg -> Handler Privilege
createOrUpdatePrivilege privilege = do
                let PrivilegeArg {..} = privilege
                now <- liftIO getCurrentTime
                entityId <- if privilegeId > 0 then
                                do
                                  let privilegeKey = (toSqlKey $ fromIntegral $ privilegeId)::Privilege_Id
                                  _ <- runDB $ update privilegeKey [ Privilege_Key =. key
                                                                   , Privilege_Name =. name
                                                                   , Privilege_Description =. description
                                                                   , Privilege_Active =. active
                                                                   , Privilege_ModifiedDate =. Just now
                                                                   ]
                                  return privilegeKey
                               else do
                                  privilegeKey <- runDB $ insert $ fromPrivilegeQL privilege now Nothing
                                  return privilegeKey
                response <- dbFetchPrivilegeById entityId
                return response

-- CONVERTERS
--     Id sql=privilege_id
--     key Text
--     name Text
--     description Text Maybe
--     active Bool
--     createdDate UTCTime
--     modifiedDate UTCTime
toPrivilegeQL :: Entity Privilege_ -> Privilege
toPrivilegeQL (Entity privilegeId privilege) = Privilege { privilegeId = fromIntegral $ fromSqlKey privilegeId
                                                         , key = privilege_Key
                                                         , name = privilege_Name
                                                         , description = privilege_Description
                                                         , active = privilege_Active
                                                         , createdDate = Just $ fromString $ show privilege_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                                      where
                                                        Privilege_ {..} = privilege
                                                        m = case privilege_ModifiedDate of
                                                              Just d -> Just $ fromString $ show d
                                                              Nothing -> Nothing

fromPrivilegeQL :: PrivilegeArg -> UTCTime -> Maybe UTCTime -> Privilege_
fromPrivilegeQL (PrivilegeArg {..}) cd md = Privilege_ { privilege_Key = key
                                                    , privilege_Name = name
                                                    , privilege_Description = description
                                                    , privilege_Active = active
                                                    , privilege_CreatedDate = cd
                                                    , privilege_ModifiedDate = md
                                                    }

{-
query {
  privileges {
    privilege(entityId: 11) {
      privilegeId
      key
      description
      active
      createdDate
      modifiedDate
    }
    list(queryString: "") {
      privilegeId
      key
      description
      active
      createdDate
      modifiedDate
    }
  }
}

mutation {
  savePrivilege(privilegeId:16, key: "test", name: "sloss", description: "option" active: true) {
    privilegeId
    key
    description
    active
    createdDate
    modifiedDate
  }
}
-}
