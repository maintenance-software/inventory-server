{-# LANGUAGE CPP                   #-}
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

module Graphql.Maintenance.SubTask.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Enums
import Graphql.Utils
import Graphql.Maintenance.SubTask.SubTaskKind

data SubTask o = SubTask { subTaskId :: Int
                         , order :: Int
                   	     , group :: Text
                   	     , description :: Maybe Text
                   	     , mandatory :: Bool
                         , createdDate :: Text
                         , modifiedDate :: Maybe Text
                   	     , subTaskKind :: Maybe(() -> o () Handler SubTaskKind)                   
                         } deriving (Generic, GQLType)

data SubTaskArg = SubTaskArg { subTaskId :: Int
                             , order :: Int
                             , group :: Text
                             , description :: Maybe Text
                             , mandatory :: Bool
                             , subTaskKindId :: Maybe Int
                             } deriving (Generic)

instance GQLType SubTaskArg where
    type  KIND SubTaskArg = INPUT_OBJECT
    description = const $ Just $ pack "This field holds SubTask Input information"

{-
query {
  inventories(queryString: "") {
    subTaskId
    name
    description
  }
}

mutation {
  saveCategory(subTaskId: 0, name: "test", description: "sss") {
    subTaskId
    name
  }
}
-}
