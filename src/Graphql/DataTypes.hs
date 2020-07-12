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
import GHC.Generics ()
import Data.Morpheus.Types (GQLType(..))
import Data.Morpheus.Kind (INPUT)
import Graphql.Utils (Page, PageArg, EntityIdArg, EntityChangeStatusArg, EntityArg)
import Graphql.Category
import Graphql.Admin.Privilege ()
import Graphql.Admin.Role ()
import Graphql.Maintenance.Task.DataTypes (Task)
import Graphql.Maintenance.TaskTrigger.DataTypes (TaskTrigger)
import Graphql.Maintenance.SubTask.DataTypes (SubTask)
import Graphql.Asset.DataTypes (InventoryItem)
import Graphql.Admin.DataTypes (Person)

-- Equipments Types START
data Equipments o = Equipments { equipment :: EntityIdArg -> o () Handler (Equipment o)
                               , page :: PageArg -> o () Handler (Page (Equipment o))
                               , saveEquipment :: EquipmentArg -> o () Handler (Equipment o)
                               , setMaintenance :: SetMaintenanceArg -> o () Handler Bool
--                               , fetchWorkQueues :: PageArg -> o () Handler (Page (Equipment o))
                               } deriving (Generic, GQLType)

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
--                             , workQueues :: () -> o () Handler [WorkQueue o]
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data EquipmentArg = EquipmentArg { equipmentId :: Int
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
                                 , parentId :: Maybe Int
                                 } deriving (Generic, GQLType)

data SetMaintenanceArg = SetMaintenanceArg { equipmentId :: Int
                                           , maintenanceId :: Int
                                           } deriving (Generic, GQLType)
-- Equipments Types End

-- WorkOrderTask Types START
data WorkQueues o = WorkQueues { fetchPendingWorkQueues :: PageArg -> o () Handler (Page (WorkQueue o))
                               , addWorkQueueDate :: WorkQueueDateArg -> o () Handler Bool
                               , addWorkQueueEvent :: WorkQueueEventArg -> o () Handler Bool
                               } deriving (Generic, GQLType)

data WorkQueue o = WorkQueue { workQueueId :: Int
                             , rescheduledDate :: Maybe Text
                             , scheduledDate :: Text
                             , incidentDate :: Maybe Text
                             , startWorkDate :: Maybe Text
                             , finishedWorkDate :: Maybe Text
                             , notes :: Maybe Text
                             , outOfServiceInterval :: Int
                             , status :: Text
                             , workType :: Text
                             , equipment :: () -> o () Handler (Equipment o)
                             , task :: () -> o () Handler (Task o)
                             , taskTrigger :: () -> o () Handler (TaskTrigger o)
                             , workOrderResources :: () -> o () Handler [WorkOrderResource o]
                             , workOrderSubTask :: () -> o () Handler [WorkOrderSubTask o]
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data WorkOrderResource o = WorkOrderResource { workOrderResourceId :: Int
                                             , amount :: Int
                                             , resourceType :: Text
                                             , employeeCategoryId :: Maybe Int
                                             , itemId :: Maybe Int
                                             , humanResource :: Maybe (() -> o () Handler (Person o))
                                             , inventoryItem :: Maybe (() -> o () Handler (InventoryItem o))
                                             , workQueue :: () -> o () Handler (WorkQueue o)
                                             , createdDate :: Text
                                             , modifiedDate :: Maybe Text
                                             } deriving (Generic, GQLType)

data WorkOrderSubTask o = WorkOrderSubTask { workOrderSubTaskId :: Int
                                           , value :: Text
                                           , subTask :: () -> o () Handler (SubTask o)
                                           , workQueue :: () -> o () Handler (WorkQueue o)
                                           , createdDate :: Text
                                           , modifiedDate :: Maybe Text
                                           } deriving (Generic, GQLType)

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
-- WorkOrderTask Types END


-- WorkOrder Types START
data WorkOrders o = WorkOrders { workOrder :: EntityIdArg ->  o () Handler (WorkOrder o)
                               , page :: PageArg -> o () Handler (Page (WorkOrder o))
                               , createUpdateWorkOrder :: WorkOrderArg -> o () Handler (WorkOrder o)
                               , changeStatus :: EntityChangeStatusArg -> o () Handler Bool
                               , saveWorkOrderProgress :: WorkOrderProgressArg -> o () Handler Bool
                               , saveWorkOrderResources :: EntityArg [WorkOrderResourceArg] -> o () Handler Bool
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
                             , generatedBy :: Maybe(() -> o () Handler (Person o))
                             , responsible :: Maybe(() -> o () Handler (Person o))
                             , parent :: Maybe (() -> o () Handler (WorkOrder o))
                             , workQueues :: () -> o () Handler [WorkQueue o]
--                             , equipments :: () -> o () Handler [Equipment o]
--                             , workOrderResources :: () -> o () Handler [WorkOrderResource o]
--                             , workOrderSubTask :: () -> o () Handler [WorkOrderSubTask o]
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)


data WorkOrderArg = WorkOrderArg { workOrderId :: Int
                                 , estimateDuration :: Int
                                 , rate :: Int
                                 , notes :: Text
                                 , generatedById :: Maybe Int
                                 , responsibleId :: Maybe Int
                                 , parentId :: Maybe Int
                                 , workQueueIds :: [Int]
                                 } deriving (Generic, GQLType)

--data WorkOrderResourceArg = WorkOrderResourceArg { workOrderId :: Int
--                                                 , resources :: [ResourceArg]
--                                                 } deriving (Generic, GQLType)

data WorkOrderResourceArg = WorkOrderResourceArg { workOrderResourceId :: Int
                               , amount :: Int
                               , resourceType :: Text
                               , employeeCategoryId :: Maybe Int
                               , humanResourceId :: Maybe Int
                               , itemId :: Maybe Int
                               , inventoryItemId :: Maybe Int
                               , workQueueTaskId :: Int
                               } deriving (Generic)

instance GQLType WorkOrderResourceArg where
    type  KIND WorkOrderResourceArg = INPUT
    description = const $ Just $ pack "The item that holds the WorkOrderResourceArg information"

data WorkOrderProgressArg = WorkOrderProgressArg { workQueueId :: Int
                                                 , workOrderId :: Int
                                                 , startWorkDate :: Text
                                                 , finishedWorkDate :: Text
                                                 , notes :: Text
                                                 , status :: Text
                                                 , workOrderSubTasks :: [WorkOrderSubTaskArg]
                                                 } deriving (Generic)

data WorkOrderSubTaskArg = WorkOrderSubTaskArg { workOrderSubTaskId :: Int
                                               , value :: Text
                                               , subTaskId :: Int
                                               } deriving (Generic)

instance GQLType WorkOrderSubTaskArg where
    type  KIND WorkOrderSubTaskArg = INPUT
    description = const $ Just $ pack "The item that holds the WorkOrderSubTaskArg information"
-- WorkOrder Types END
