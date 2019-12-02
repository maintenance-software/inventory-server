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

module Graphql.Privilege (PrivilegeQL, PrivilegeArgs, resolvePrivilege) where

import Import
import GHC.Generics
import Data.Morpheus.Types (GQLType(..), lift, Res)
import Database.Persist.Sql (toSqlKey, fromSqlKey)

data PrivilegeQL = PrivilegeQL { privilegeId :: Int
                               , name :: Text
                               , description :: Maybe Text
                               , active :: Bool
                               } deriving (Generic, GQLType)

data PrivilegeArgs = PrivilegeArgs { privilegeId :: Int } deriving (Generic)

--data PrivilegeQL m = PrivilegeQL { privilege :: PrivilegeArgs -> m (Entity Privilege) } deriving (Generic, GQLType)

dbFetchPrivilegeById:: PrivilegeId -> Handler PrivilegeQL
dbFetchPrivilegeById privilegeId = do
                                      privilege <- runDB $ getJustEntity privilegeId
                                      return $ toPrivilegeQL privilege


resolvePrivilege :: PrivilegeArgs -> Res e Handler PrivilegeQL
resolvePrivilege PrivilegeArgs { privilegeId } = lift $ dbFetchPrivilegeById privilegeKey
                                              where
                                                privilegeKey = (toSqlKey $ fromIntegral $ privilegeId)::PrivilegeId


toPrivilegeQL :: Entity Privilege -> PrivilegeQL
toPrivilegeQL (Entity privilegeId privilege) = PrivilegeQL { privilegeId = fromIntegral $ fromSqlKey privilegeId
                                                           , name = a
                                                           , description = b
                                                           , active = c
                                                           }
                                                      where
                                                        Privilege a b c = privilege
