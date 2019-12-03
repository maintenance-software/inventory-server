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

module Graphql.Privilege (AbstractPrivilegeQL, resolvePrivilege) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Graphql.Utils

data PrivilegeQL = PrivilegeQL { privilegeId :: Int
                               , name :: Text
                               , description :: Maybe Text
                               , active :: Bool
                               } deriving (Generic, GQLType)

data AbstractPrivilegeQL m = AbstractPrivilegeQL { findById :: FindByIdArgs -> m PrivilegeQL
                                                 , list :: ListArgs -> m [PrivilegeQL]
                                                 } deriving (Generic, GQLType)

data FindByIdArgs = FindByIdArgs { privilegeId :: Int } deriving (Generic)

data ListArgs = ListArgs { queryString :: Text, pageable :: Maybe Pageable } deriving (Generic)

dbFetchPrivilegeById:: PrivilegeId -> Handler PrivilegeQL
dbFetchPrivilegeById privilegeId = do
                                      privilege <- runDB $ getJustEntity privilegeId
                                      return $ toPrivilegeQL privilege

dbFetchPrivileges:: ListArgs -> Handler [PrivilegeQL]
dbFetchPrivileges ListArgs { queryString, pageable } = do
                                                        privileges <- runDB $ selectList [] [Asc PrivilegeId, LimitTo size, OffsetBy $ (page - 1) * size]
                                                        return $ P.map toPrivilegeQL privileges
                                                  where
                                                    (page, size) = case pageable of
                                                                    Just (Pageable x y) -> (x, y)
                                                                    Nothing -> (1, 10)

findByIdResolver :: FindByIdArgs -> Res e Handler PrivilegeQL
findByIdResolver FindByIdArgs { privilegeId } = lift $ dbFetchPrivilegeById privilegeKey
                                              where
                                                privilegeKey = (toSqlKey $ fromIntegral $ privilegeId)::PrivilegeId

listResolver :: ListArgs -> Res e Handler [PrivilegeQL]
listResolver listArgs = lift $ dbFetchPrivileges listArgs

resolvePrivilege :: AbstractPrivilegeQL (Res () Handler)
resolvePrivilege = AbstractPrivilegeQL {  findById = findByIdResolver, list = listResolver }

{- Converters -}
toPrivilegeQL :: Entity Privilege -> PrivilegeQL
toPrivilegeQL (Entity privilegeId privilege) = PrivilegeQL { privilegeId = fromIntegral $ fromSqlKey privilegeId
                                                           , name = a
                                                           , description = b
                                                           , active = c
                                                           }
                                                      where
                                                        Privilege a b c = privilege
