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

module Graphql.Privilege (Privileges, Privilege, resolvePrivilege, resolveSavePrivilege, toPrivilegeQL) where

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

data Privileges m = Privileges { findById :: FindByIdArgs -> m Privilege
                               , list :: ListArgs -> m [Privilege]
                               } deriving (Generic, GQLType)

data FindByIdArgs = FindByIdArgs { privilegeId :: Int } deriving (Generic)

-- data ListArgs = ListArgs { queryString :: Text, pageable :: Maybe Pageable } deriving (Generic)

-- DB ACTIONS
dbFetchPrivilegeById:: Privilege_Id -> Handler Privilege
dbFetchPrivilegeById privilegeId = do
                                      privilege <- runDB $ getJustEntity privilegeId
                                      return $ toPrivilegeQL privilege

dbFetchPrivileges:: ListArgs -> Handler [Privilege]
dbFetchPrivileges ListArgs {..} = do
                                  privileges <- runDB $ selectList [] [Asc Privilege_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                                  return $ P.map toPrivilegeQL privileges
                              where
                                (page, size) = case pageable of
                                                Just (Pageable x y) -> (x, y)
                                                Nothing -> (1, 10)

-- Query Resolvers
findByIdResolver :: FindByIdArgs -> Res e Handler Privilege
findByIdResolver FindByIdArgs { privilegeId } = lift $ dbFetchPrivilegeById privilegeKey
                                              where
                                                privilegeKey = (toSqlKey $ fromIntegral $ privilegeId)::Privilege_Id

listResolver :: ListArgs -> Res e Handler [Privilege]
listResolver listArgs = lift $ dbFetchPrivileges listArgs

resolvePrivilege :: Privileges (Res () Handler)
resolvePrivilege = Privileges {  findById = findByIdResolver, list = listResolver }

-- Mutation Resolvers
resolveSavePrivilege :: Privilege -> MutRes e Handler Privilege
resolveSavePrivilege arg = lift $ createOrUpdatePrivilege arg

createOrUpdatePrivilege :: Privilege -> Handler Privilege
createOrUpdatePrivilege privilege = do
                let Privilege {..} = privilege
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

fromPrivilegeQL :: Privilege -> UTCTime -> Maybe UTCTime -> Privilege_
fromPrivilegeQL (Privilege {..}) cd md = Privilege_ { privilege_Key = key
                                                    , privilege_Name = name
                                                    , privilege_Description = description
                                                    , privilege_Active = active
                                                    , privilege_CreatedDate = cd
                                                    , privilege_ModifiedDate = md
                                                    }
