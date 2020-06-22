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

module Graphql.Maintenance.Task.DataTypes where

import Import
import GHC.Generics ()
import Data.Morpheus.Kind (INPUT)
import Data.Morpheus.Types (GQLType(..))
import Enums ()
import Graphql.Category
import Graphql.Maintenance.Task.TaskResource
import Graphql.Maintenance.SubTask.DataTypes
import Graphql.Maintenance.TaskTrigger.DataTypes

data Task o = Task { taskId :: Int
                   , name :: Text
                   , description :: Maybe Text
                   , priority :: Int
                   , duration :: Int
                   , downTimeDuration :: Int
                   , attribute1 :: Maybe Text
                   , attribute2 :: Maybe Text
                   , maintenanceId :: Maybe Int
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   , taskCategory :: Maybe(() -> o () Handler Category)
                   , subTasks :: () -> o () Handler [SubTask o]
                   , taskTriggers :: () -> o () Handler [TaskTrigger o]
                   , taskResources :: () -> o () Handler [TaskResource o]
                   } deriving (Generic, GQLType)

data TaskArg = TaskArg { taskId :: Int
                       , name :: Text
                       , description :: Maybe Text
                       , priority :: Int
                       , duration :: Int
                       , downTimeDuration :: Int
                       , attribute1 :: Maybe Text
                       , attribute2 :: Maybe Text
                       , taskCategoryId :: Maybe Int
                       , subTasks :: [SubTaskArg]
                       , taskTriggers :: [TaskTriggerArg]
                       , taskResources :: [TaskResourceArg]
                       } deriving (Generic)

instance GQLType TaskArg where
    type  KIND TaskArg = INPUT
    description = const $ Just $ pack "This field holds Task Input information"


{-
query fetchMaintenancePlans {
      maintenances {
         task (entityId: 43) {
          taskResources {
          	taskResourceId
            inventoryResource {
              name
            }
        }
        }
      }
   }

   mutation {
     maintenances  {
   		createUpdateTasks (
         maintenanceId: 1,
         tasks: [{
          taskId: 0,
          name: ": Text",
          priority: 0,
          duration: 8,
          downTimeDuration: 12,
          subTasks: []
          taskTriggers: []
          taskResources: [{
           taskResourceId: 0,
           order: 0,
           amount: 9,
           resourceType: "INVENTORY",
           unitId: 2,
           inventoryResourceId: 5
         }]
         }]
       ){
         	taskId
       }
     }
   }
-}
