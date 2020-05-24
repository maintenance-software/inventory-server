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

module Graphql.Maintenance.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Types (GQLType)
import Graphql.Utils (GetEntityByIdArg, Page, PageArg)
import Graphql.Admin.DataTypes
import Graphql.Maintenance.Task.DataTypes
import Graphql.Asset.Equipment.DataTypes
import Graphql.Category

data Maintenance o = Maintenance { maintenanceId :: Int
                                 , name :: Text
                                 , description :: Maybe Text
                                 , status :: Text
                                 , createdDate :: Text
                                 , modifiedDate :: Maybe Text
                                 , tasks :: () -> o () Handler [Task o]
                                 , equipments :: () -> o () Handler [Equipment o]
                                 } deriving (Generic, GQLType)

data TaskActivity = TaskActivity { taskActivityId :: Int
                                 , scheduledDate :: Maybe Text
                                 , calculatedDate :: Text
                                 , incidentDate :: Maybe Text
                                 , rescheduled :: Bool
                                 , status :: Text
                                 , assetId :: Int
                                 , assetCode :: Text
                                 , assetName :: Text
                                 , maintenanceId :: Maybe Int
                                 , maintenanceName :: Maybe Text
                                 , taskId :: Int
                                 , taskName :: Text
                                 , taskPriority :: Int
                                 , taskTriggerId :: Int
                                 , triggerDescription :: Text
                                 , taskType :: Text
                                 , createdDate :: Text
                                 } deriving (Generic, GQLType)

data WorkOrder o = WorkOrder { workOrderId :: Int
                             , workOrderCode :: Text
                             , workOrderStatus :: Text
                             , estimateDuration :: Int
                             , executionDuration :: Int
                             , rate :: Int
                             , totalCost :: Float
                             , percentage :: Float
                             , notes :: Text
                             , generatedBy :: () -> o () Handler (Person o)
                             , responsible :: () -> o () Handler (Person o)
                             , parent :: Maybe (() -> o () Handler (WorkOrder o))
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data Maintenances o = Maintenances { maintenance :: GetEntityByIdArg ->  o () Handler (Maintenance o)
                                   , page :: PageArg -> o () Handler (Page (Maintenance o))
                                   , availableEquipments :: PageArg -> o () Handler (Page (Equipment o))
                                   , taskActivities :: PageArg -> o () Handler (Page TaskActivity)
                                   , addTaskActivityDate :: TaskActivityDateArg -> o () Handler Bool
                                   , addTaskActivityEvent :: TaskActivityEventArg -> o () Handler Bool
                                   , saveMaintenance :: MaintenanceArg -> o () Handler (Maintenance o)
                                   , createUpdateTasks :: MaintenanceTaskArg -> o () Handler [Task o]
                                   , task :: GetEntityByIdArg -> o () Handler (Task o)
                                   , equipmentTasks :: GetEntityByIdArg -> o () Handler [Task o]
                                   , workOrder :: GetEntityByIdArg -> o () Handler (WorkOrder o)
                                   , workOrders :: PageArg -> o () Handler (Page (WorkOrder o))
                                   , createUpdateWorkOrder :: WorkOrderArg -> o () Handler (WorkOrder o)
                                   } deriving (Generic, GQLType)

data MaintenanceArg = MaintenanceArg { maintenanceId :: Int
                                     , name :: Text
                                     , description :: Maybe Text
                                     , status :: Text
                                     } deriving (Generic)

data MaintenanceTaskArg = MaintenanceTaskArg { maintenanceId :: Maybe Int
                                             , tasks :: [TaskArg]
                                             } deriving (Generic)

data TaskActivityDateArg = TaskActivityDateArg { lastMaintenanceDate :: Text
                                               , assetId :: Int
                                               , maintenanceId :: Int
                                               } deriving (Generic)

data TaskActivityEventArg = TaskActivityEventArg { assetId :: Int
                                                 , taskId :: Int
                                                 , taskTriggerId :: Int
                                                 , maintenanceId :: Maybe Int
                                                 , reportedById :: Int
                                                 , hasAssetFailure :: Bool
                                                 , incidentDate :: Maybe Text
                                                 } deriving (Generic)

data WorkOrderArg = WorkOrderArg { workOrderId :: Int
                                 , workOrderStatus :: Text
                                 , estimateDuration :: Int
                                 , executionDuration :: Int
                                 , rate :: Int
                                 , notes :: Text
                                 , generatedById :: Int
                                 , responsibleId :: Int
                                 , parentId :: Maybe Int
                                 , activityIds :: [Int]
                                 } deriving (Generic, GQLType)
