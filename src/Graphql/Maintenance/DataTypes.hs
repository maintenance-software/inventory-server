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
import GHC.Generics ()
import Data.Morpheus.Kind (INPUT)
import Data.Morpheus.Types (GQLType(..))
import Graphql.Utils (EntityIdArg, Page, PageArg)
import Graphql.Admin.DataTypes
import Graphql.Maintenance.Task.DataTypes
import Graphql.Asset.Equipment.DataTypes ()
import Graphql.Asset.DataTypes (InventoryItem)
import Graphql.Category ()
import Graphql.Utils ()
import Graphql.DataTypes (Equipment, WorkQueue)

data Maintenance o = Maintenance { maintenanceId :: Int
                                 , name :: Text
                                 , description :: Maybe Text
                                 , status :: Text
                                 , createdDate :: Text
                                 , modifiedDate :: Maybe Text
                                 , tasks :: () -> o () Handler [Task o]
                                 , equipments :: () -> o () Handler [Equipment o]
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
                             , workQueues :: () -> o () Handler [Equipment o]
                             , workOrderResources :: () -> o () Handler [WorkOrderResource o]
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data Maintenances o = Maintenances { maintenance :: EntityIdArg ->  o () Handler (Maintenance o)
                                   , page :: PageArg -> o () Handler (Page (Maintenance o))
                                   , availableEquipments :: PageArg -> o () Handler (Page (Equipment o))
                                   , addWorkQueueDate :: WorkQueueDateArg -> o () Handler Bool
                                   , addWorkQueueEvent :: WorkQueueEventArg -> o () Handler Bool
                                   , saveMaintenance :: MaintenanceArg -> o () Handler (Maintenance o)
                                   , createUpdateTasks :: MaintenanceTaskArg -> o () Handler [Task o]
                                   , task :: EntityIdArg -> o () Handler (Task o)
                                   , equipmentTasks :: EntityIdArg -> o () Handler [Task o]
                                   , workOrder :: EntityIdArg -> o () Handler (WorkOrder o)
                                   , workOrders :: PageArg -> o () Handler (Page (WorkOrder o))
                                   , createUpdateWorkOrder :: WorkOrderArg -> o () Handler (WorkOrder o)
--                                   , woPreResources :: EntityIdsArg -> o () Handler [WoAssets]
                                   } deriving (Generic, GQLType)

data MaintenanceArg = MaintenanceArg { maintenanceId :: Int
                                     , name :: Text
                                     , description :: Maybe Text
                                     , status :: Text
                                     } deriving (Generic)

data MaintenanceTaskArg = MaintenanceTaskArg { maintenanceId :: Maybe Int
                                             , tasks :: [TaskArg]
                                             } deriving (Generic)

data WorkQueueDateArg = WorkQueueDateArg { lastMaintenanceDate :: Text
                                               , assetId :: Int
                                               , maintenanceId :: Int
                                               } deriving (Generic)

data WorkQueueEventArg = WorkQueueEventArg { assetId :: Int
                                                 , taskId :: Int
                                                 , taskTriggerId :: Int
                                                 , maintenanceId :: Maybe Int
                                                 , reportedById :: Int
                                                 , hasAssetFailure :: Bool
                                                 , incidentDate :: Maybe Text
                                                 } deriving (Generic)

data WorkOrderArg = WorkOrderArg { workOrderId :: Int
                                 , estimateDuration :: Int
                                 , rate :: Int
                                 , notes :: Text
                                 , generatedById :: Int
                                 , responsibleId :: Int
                                 , parentId :: Maybe Int
                                 , workQueueIds :: [Int]
                                 , resources :: [WorkOrderResourceArg]
                                 } deriving (Generic, GQLType)

data WorkOrderResourceArg = WorkOrderResourceArg { workOrderResourceId :: Int
                                                 , amount :: Int
                                                 , humanResourceId :: Maybe Int
                                                 , inventoryItemId :: Maybe Int
                                                 , workQueueTaskId :: Int
                                                 } deriving (Generic)

instance GQLType WorkOrderResourceArg where
    type  KIND WorkOrderResourceArg = INPUT
    description = const $ Just $ pack "The item that holds the WorkOrderResourceArg information"

data WorkOrderResource o = WorkOrderResource { workOrderResourceId :: Int
                                             , amount :: Int
                                             , humanResource :: Maybe (() -> o () Handler (Person o))
                                             , inventoryItem :: Maybe (() -> o () Handler (InventoryItem o))
                                             , workQueue :: () -> o () Handler (WorkQueue o)
                                             , createdDate :: Text
                                             , modifiedDate :: Maybe Text
                                             } deriving (Generic, GQLType)
