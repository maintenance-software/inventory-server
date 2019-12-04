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
{-# LANGUAGE RecordWildCards #-}

module Graphql.Privilege (Privileges, Privilege, resolvePrivilege, resolveSavePrivilege) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
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

data ListArgs = ListArgs { queryString :: Text, pageable :: Maybe Pageable } deriving (Generic)

dbFetchPrivilegeById:: Privilege_Id -> Handler Privilege
dbFetchPrivilegeById privilegeId = do
                                      privilege <- runDB $ getJustEntity privilegeId
                                      return $ toPrivilegeQL privilege

dbFetchPrivileges:: ListArgs -> Handler [Privilege]
dbFetchPrivileges ListArgs { queryString, pageable } = do
                                                        privileges <- runDB $ selectList [] [Asc Privilege_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                                                        return $ P.map toPrivilegeQL privileges
                                                  where
                                                    (page, size) = case pageable of
                                                                    Just (Pageable x y) -> (x, y)
                                                                    Nothing -> (1, 10)

findByIdResolver :: FindByIdArgs -> Res e Handler Privilege
findByIdResolver FindByIdArgs { privilegeId } = lift $ dbFetchPrivilegeById privilegeKey
                                              where
                                                privilegeKey = (toSqlKey $ fromIntegral $ privilegeId)::Privilege_Id

listResolver :: ListArgs -> Res e Handler [Privilege]
listResolver listArgs = lift $ dbFetchPrivileges listArgs

resolvePrivilege :: Privileges (Res () Handler)
resolvePrivilege = Privileges {  findById = findByIdResolver, list = listResolver }

-- MUTATION resolvers
resolveSavePrivilege :: Privilege -> MutRes e Handler Privilege
resolveSavePrivilege arg = lift $ createOrUpdatePrivilege arg

createOrUpdatePrivilege :: Privilege -> Handler Privilege
createOrUpdatePrivilege privilege = do
                let Privilege a b c d e f g = privilege
                privilegeId <- if a > 0 then
                                do
                                  let privilegeKey = (toSqlKey $ fromIntegral $ a)::Privilege_Id
                                  _ <- runDB $ update privilegeKey [ Privilege_Name =. c
                                                                   , Privilege_Active =. e
                                                                   ]
                                  return privilegeKey
                               else do
                                  time <- liftIO getCurrentTime
                                  privilegeKey <- runDB $ insert $ fromPrivilegeQL privilege time
                                  return privilegeKey
                response <- dbFetchPrivilegeById privilegeId
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
                                                           , name = b
                                                           , description = c
                                                           , active = d
                                                           , createdDate = Just $ fromString $ show e
                                                           }
                                                      where
                                                        Privilege_ a b c d e f = privilege

fromPrivilegeQL :: Privilege -> UTCTime -> Privilege_
fromPrivilegeQL Privilege {..} date = Privilege_ key name description active date  (Just date)


localDay :: IO Day
localDay = fmap utctDay getCurrentTime
