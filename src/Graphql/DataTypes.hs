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

module Graphql.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..))
import Graphql.Utils (Page, PageArg, EntityIdArg, EntityChangeStatusArg)
import Graphql.Category
import Graphql.Admin.Privilege
import Graphql.Admin.Role
import Graphql.Maintenance.Task.DataTypes (Task)
import Graphql.Maintenance.TaskTrigger.DataTypes (TaskTrigger)

data Equipment o = Equipment { equipmentId :: Int
                             , name :: Text
                             , description :: Maybe Text
                             , code :: Text
                             , partNumber :: Maybe Text
                             , manufacturer :: Maybe Text
                             , model :: Maybe Text
                             , notes:: Maybe Text
                             , status :: Text
                             , images :: [Text]
                             , priority :: Int
                             , hoursAverageDailyUse :: Int
                             , outOfService :: Bool
                             , purchaseDate :: Maybe Text
                             , children :: PageArg -> o () Handler (Page (Equipment o))
                             , parent :: Maybe(() -> o () Handler (Equipment o))
                             , category :: Maybe(() -> o () Handler Category)
                             , workQueues :: () -> o () Handler [WorkQueue o]
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data WorkQueue o = WorkQueue { workQueueId :: Int
                             , rescheduledDate :: Maybe Text
                             , scheduledDate :: Text
                             , incidentDate :: Maybe Text
                             , status :: Text
                             , workType :: Text
                             , task :: () -> o () Handler (Task o)
                             , taskTrigger :: () -> o () Handler (TaskTrigger o)
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)
