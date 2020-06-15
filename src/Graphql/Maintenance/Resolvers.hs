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

module Graphql.Maintenance.Resolvers (
      maintenanceResolver
    , getMaintenanceByIdResolver_
    , saveMaintenanceResolver
    , toMaintenanceQL
    , toWorkQueueQL
    , workQueueByEquipmentIdResolver_
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums ()
import Graphql.Utils
import Graphql.Maintenance.TaskTrigger.Resolvers (getTaskTriggerByIdResolver_)
import Graphql.Maintenance.Task.Resolvers (toTaskQL, getTaskByIdResolver, getTaskByIdResolver_, taskResolver_)
import Graphql.Maintenance.Task.Persistence
import Graphql.Maintenance.DataTypes
import Graphql.Maintenance.Persistence
import Graphql.DataTypes (WorkQueue(..), Equipment(..))
import Graphql.Admin.Person (getPersonByIdResolver_)
import Graphql.Maintenance.Task.DataTypes (Task(..))
import Graphql.Asset.Equipment.Resolvers (toEquipmentQL)
import Graphql.Asset.InventoryItem.Resolvers (getInventoryItemByIdResolver_)

maintenanceResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (Maintenances o)
maintenanceResolver _ = pure Maintenances { maintenance = getMaintenanceByIdResolver
                                          , page = maintenancePageResolver
                                          , availableEquipments = availableEquipmentPageResolver
                                          , equipmentTasks = equipmentTasksResolver
                                          , addWorkQueueDate = addWorkQueueDateResolver
                                          , addWorkQueueEvent = addWorkQueueEventResolver
                                          , saveMaintenance = saveMaintenanceResolver
                                          , task = getTaskByIdResolver
                                          , createUpdateTasks = createUpdateTasksResolver
                                          , workOrder = getWorkOrderByIdResolver
                                          , createUpdateWorkOrder = createUpdateWorkOrderResolver
                                          , workOrders = workOrderPageResolver
--                                          , woPreResources = woPreResourcesResolver
--                                          , eventTriggers = listEventTriggerResolver
--                                          , saveEventTrigger = saveEventTriggerResolver
                                          }

getMaintenanceByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (Maintenance o)
getMaintenanceByIdResolver EntityIdArg {..} = lift $ do
                                              let maintenanceId = (toSqlKey $ fromIntegral $ entityId)::Maintenance_Id
                                              maintenance <- runDB $ getJustEntity maintenanceId
                                              return $ toMaintenanceQL maintenance

getMaintenanceByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Maintenance_Id -> () -> o () Handler (Maintenance o)
getMaintenanceByIdResolver_ maintenanceId _ = lift $ do
                                    maintenance <- runDB $ getJustEntity maintenanceId
                                    return $ toMaintenanceQL maintenance

getWorkOrderByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (WorkOrder o)
getWorkOrderByIdResolver EntityIdArg {..} = lift $ do
                                              let workOrderId = (toSqlKey $ fromIntegral $ entityId)::WorkOrder_Id
                                              workOrder <- runDB $ getJustEntity workOrderId
                                              return $ toWorkOrderQL workOrder

getWorkOrderByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler (WorkOrder o)
getWorkOrderByIdResolver_ workOrderId _ = lift $ do
                                              workOrder <- runDB $ getJustEntity workOrderId
                                              return $ toWorkOrderQL workOrder

maintenancePageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (Maintenance o))
maintenancePageResolver page = lift $ do
                        countItems <- maintenanceQueryCount page
                        queryResult <- maintenanceQuery page
                        let result = P.map (\ m -> toMaintenanceQL m) queryResult
                        return Page { totalCount = countItems
                                    , content = result
                                    , pageInfo = PageInfo { hasNext = (pageIndex_ * pageSize_ + pageSize_ < countItems)
                                                          , hasPreview = pageIndex_ * pageSize_ > 0
                                                          , pageSize = pageSize_
                                                          , pageIndex = pageIndex_
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex_ = case pageIndex of Just  x  -> x; Nothing -> 0
                            pageSize_ = case pageSize of Just y -> y; Nothing -> 10

availableEquipmentPageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (Equipment o))
availableEquipmentPageResolver page = lift $ do
                        countItems <- availableEquipmentQueryCount page
                        queryResult <- availableEquipmentQuery page
                        let result = P.map (\(e, i) -> toEquipmentQL e i) queryResult
                        return Page { totalCount = countItems
                                    , content = result
                                    , pageInfo = PageInfo { hasNext = (pageIndex_ * pageSize_ + pageSize_ < countItems)
                                                          , hasPreview = pageIndex_ * pageSize_ > 0
                                                          , pageSize = pageSize_
                                                          , pageIndex = pageIndex_
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex_ = case pageIndex of Just  x  -> x; Nothing -> 0
                            pageSize_ = case pageSize of Just y -> y; Nothing -> 10

workOrderPageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (WorkOrder o))
workOrderPageResolver page = lift $ do
                        countItems <- workOrderQueryCount page
                        queryResult <- workOrderQuery page
                        let result = P.map (\ wo -> toWorkOrderQL wo) queryResult
                        return Page { totalCount = countItems
                                    , content = result
                                    , pageInfo = PageInfo { hasNext = (pageIndex_ * pageSize_ + pageSize_ < countItems)
                                                          , hasPreview = pageIndex_ * pageSize_ > 0
                                                          , pageSize = pageSize_
                                                          , pageIndex = pageIndex_
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex_ = case pageIndex of Just x -> x; Nothing -> 0
                            pageSize_ = case pageSize of Just y -> y; Nothing -> 10


equipmentResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Maintenance_Id -> () -> o () Handler [Equipment o]
equipmentResolver_ maintenanceId _ = lift $ do
                              itemEquipments <- equipmentQuery maintenanceId
                              let result = P.map (\(e, i) -> toEquipmentQL e i) itemEquipments
                              return result

equipmentTasksResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => EntityIdArg -> t Handler [Task o]
equipmentTasksResolver EntityIdArg {..} = lift $ do
                         let itemId = (toSqlKey $ fromIntegral $ entityId)::Item_Id
                         let equipmentKey = Equipment_Key {unEquipment_Key  = itemId}
                         Entity _ Equipment_{..} <- runDB $ getJustEntity equipmentKey
                         entityTasks <- case equipment_MaintenanceId of
                                    Nothing -> pure []
                                    Just maintenanceId -> taskQuery maintenanceId
                         return $ P.map (\t -> toTaskQL t) entityTasks

getWorkQueueByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkQueue_Id -> () -> o () Handler (WorkQueue o)
getWorkQueueByIdResolver_ workQueueId _ = lift $ do
                              workQueue <- runDB $ getJustEntity workQueueId
                              return $ toWorkQueueQL workQueue


workQueueByEquipmentIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Item_Id -> () -> o () Handler [WorkQueue o]
workQueueByEquipmentIdResolver_ equipmentId _ = lift $ do
                              workQueues <- fetchPendingWorkQueueByEquipmentIdQuery equipmentId
                              let result = P.map (\ w -> toWorkQueueQL w) workQueues
                              return result

fetchWorkQueuesByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [Equipment o]
fetchWorkQueuesByWorkOrderIdResolver_ workOrderId _ = lift $ do
                              workQueues <- fetchWorkQueuesByWorkOrderIdQuery workOrderId
                              let result = P.map (\ (e, i) -> toEquipmentQL e i) workQueues
                              return result

fetchWorkResourcesByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [WorkOrderResource o]
fetchWorkResourcesByWorkOrderIdResolver_ workOrderId _ = lift $ do
                              workOrderResources <-  runDB $ selectList [WorkOrderResource_WorkOrderId ==. workOrderId] []
                              return $ P.map (\ r -> toWorkOrderResourceQL r) workOrderResources

--resourceRequirementResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => WoResourceRequirement -> t Handler [WoAssets]
--woPreResourcesResolver requestArg = lift $ do
--                       equipmentIds <- workQueueCountTasksQuery requestArg
--                       equipments <- runDB $ selectList [Item_Id <-. (P.map (\(a, _) -> a) equipmentIds)] []
--                       tasks <- runDB $ selectList [Task_Id <-. (P.concat $ P.map (\(_, b) -> b) equipmentIds)] []
--                       let findAssetById = \assetId -> (P.filter (\(Entity i _) -> i == assetId) equipments)
--                       let findTasksByIds = \taskIds -> (P.filter (\ (Entity i _) -> i `P.elem` taskIds) tasks)
--                       let response = P.map (\(itemId, taskIds) -> (toWOAsset (P.head $ findAssetById itemId) (findTasksByIds taskIds))) equipmentIds
--                       return response

--toWOAsset :: Entity Item_ -> [Entity Task_] -> WoAssets
--toWOAsset (Entity itemId Item_{..}) tasks = WoAssets { assetId = fromIntegral $ fromSqlKey itemId
--                                                     , name = item_Name
--                                                     , tasks = P.map toWOTask tasks
--                                                     }

--toWOTask :: Entity Task_ -> WoAssetTask
--toWOTask (Entity taskId Task_{..}) = WoAssetTask { taskId = fromIntegral $ fromSqlKey taskId
--                                                 , name = task_Name
--                                                 , requiredResource = True
--                                                 }

saveMaintenanceResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => MaintenanceArg -> t Handler (Maintenance o)
saveMaintenanceResolver arg = lift $ do
                                  maintenanceId <- createOrUpdateMaintenance arg
                                  maintenance <- runDB $ getJustEntity maintenanceId
                                  return $ toMaintenanceQL maintenance

createUpdateTasksResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => MaintenanceTaskArg -> t Handler [Task o]
createUpdateTasksResolver MaintenanceTaskArg {..} = lift $ do
                         let entityId = case maintenanceId of Nothing -> Nothing; Just x -> Just ((toSqlKey $ fromIntegral $ x)::Maintenance_Id)
                         taskIds <- saveTasks entityId tasks
                         entityTasks <- getTaskByIds taskIds
                         return $ P.map (\t -> toTaskQL t) entityTasks

addWorkQueueDateResolver :: (MonadTrans t) => WorkQueueDateArg -> t Handler Bool
addWorkQueueDateResolver arg = lift $ do
                         workQueueSuccess <- addDateWorkQueuePersistent arg
                         return $ workQueueSuccess

addWorkQueueEventResolver :: (MonadTrans t) => WorkQueueEventArg -> t Handler Bool
addWorkQueueEventResolver arg = lift $ do
                         workQueueSuccess <- addEventWorkQueuePersistent arg
                         return $ workQueueSuccess

createUpdateWorkOrderResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrderArg -> o () Handler (WorkOrder o)
createUpdateWorkOrderResolver arg = lift $ do
                         workOrderId <- createUpdateWorkOrderPersistent arg
                         workOrder <-  runDB $ getJustEntity workOrderId
                         return $ toWorkOrderQL workOrder

-- CONVERTERS
toMaintenanceQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Maintenance_ -> Maintenance o
toMaintenanceQL (Entity maintenanceId maintenance) = Maintenance { maintenanceId = fromIntegral $ fromSqlKey maintenanceId
                                                                 , name = maintenance_Name
                                                                 , description = maintenance_Description
                                                                 , status = T.pack $ show maintenance_Status
                                                                 , tasks = taskResolver_ maintenanceId
                                                                 , equipments = equipmentResolver_ maintenanceId
                                                                 , createdDate = fromString $ show maintenance_CreatedDate
                                                                 , modifiedDate = m
                                                                 }
                                          where
                                            Maintenance_ {..} = maintenance
                                            m = case maintenance_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

toWorkQueueQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) =>Entity WorkQueue_ -> WorkQueue o
toWorkQueueQL (Entity workQueueId workQueue) = WorkQueue { workQueueId = fromIntegral $ fromSqlKey workQueueId
                                                         , rescheduledDate = case workQueue_RescheduledDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , scheduledDate = fromString $ show workQueue_ScheduledDate
                                                         , incidentDate = case workQueue_IncidentDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , status = workQueue_Status
                                                         , workType = workQueue_WorkType
                                                         , task = getTaskByIdResolver_ workQueue_TaskId
                                                         , taskTrigger = getTaskTriggerByIdResolver_ workQueue_TaskTriggerId
                                                         , createdDate = T.pack $ show workQueue_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            WorkQueue_ {..} = workQueue
                                            m = case workQueue_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

toWorkOrderQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrder_ -> WorkOrder o
toWorkOrderQL (Entity workOrderId workOrder) = WorkOrder { workOrderId = fromIntegral $ fromSqlKey workOrderId
                                                         , workOrderCode = workOrder_WorkOrderCode
                                                         , workOrderStatus = workOrder_WorkOrderStatus
                                                         , estimateDuration = workOrder_EstimateDuration
                                                         , executionDuration = workOrder_ExecutionDuration
                                                         , rate = workOrder_Rate
                                                         , totalCost = realToFrac workOrder_TotalCost
                                                         , percentage = realToFrac workOrder_Percentage
                                                         , notes = workOrder_Notes
                                                         , generatedBy = getPersonByIdResolver_ workOrder_GeneratedById
                                                         , responsible = getPersonByIdResolver_ workOrder_ResponsibleId
                                                         , parent = (case workOrder_ParentId of Nothing -> Nothing; Just a -> Just $ getWorkOrderByIdResolver_ a)
                                                         , workQueues = fetchWorkQueuesByWorkOrderIdResolver_ workOrderId
                                                         , workOrderResources = fetchWorkResourcesByWorkOrderIdResolver_ workOrderId
                                                         , createdDate = fromString $ show workOrder_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            WorkOrder_ {..} = workOrder
                                            m = case workOrder_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing


toWorkOrderResourceQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrderResource_ -> WorkOrderResource o
toWorkOrderResourceQL (Entity workOrderResourceId workOrderResource) = WorkOrderResource { workOrderResourceId = fromIntegral $ fromSqlKey workOrderResourceId
                                                                                         , amount = workOrderResource_Amount
                                                                                         , humanResource =  (case workOrderResource_HumanResourceId of Nothing -> Nothing; Just a -> Just $ getPersonByIdResolver_ a)
                                                                                         , inventoryItem =  (case workOrderResource_InventoryItemId of Nothing -> Nothing; Just a -> Just $ getInventoryItemByIdResolver_ a)
                                                                                         , workQueue = getWorkQueueByIdResolver_ workOrderResource_WorkQueueId
                                                                                         , createdDate = fromString $ show workOrderResource_CreatedDate
                                                                                         , modifiedDate = m
                                                                                         }
                                                                        where
                                                                          WorkOrderResource_ {..} = workOrderResource
                                                                          m = case workOrderResource_ModifiedDate of
                                                                                Just d -> Just $ fromString $ show d
                                                                                Nothing -> Nothing

{-
query {
  inventories(queryString: "") {
    maintenanceId
    name
    description
  }
}

mutation {
  saveCategory(maintenanceId: 0, name: "test", description: "sss") {
    maintenanceId
    name
  }
}
-}
