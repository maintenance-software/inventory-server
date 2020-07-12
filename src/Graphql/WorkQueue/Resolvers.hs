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

module Graphql.WorkQueue.Resolvers (
      toWorkQueueQL
    , workQueueResolver
    , workQueueByEquipmentIdResolver_
    , getWorkQueueByIdResolver_
) where

import Import
import Database.Persist.Sql (fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums ()
import Graphql.Utils
import Graphql.Maintenance.TaskTrigger.Resolvers (getTaskTriggerByIdResolver_)
import Graphql.Asset.InventoryItem.Resolvers (getInventoryItemByIdResolver_)
import Graphql.Maintenance.SubTask.Resolvers (getSubTaskByIdResolver_)
import Graphql.Admin.Person (getPersonByIdResolver_)
import Graphql.Maintenance.Task.Resolvers (getTaskByIdResolver_)
--import Graphql.Maintenance.Task.Persistence
import Graphql.DataTypes
import Graphql.WorkQueue.Persistence
import Graphql.Asset.Equipment.Resolvers (getEquipmentByIdResolver_)
--import Graphql.DataTypes (WorkQueue(..), Equipment(..), WorkOrderSubTask(..), WorkOrderResource(..))
--import Graphql.Admin.Person (getPersonByIdResolver_)
--import Graphql.Maintenance.Task.DataTypes (Task(..))
--import Graphql.Asset.Equipment.Resolvers (toEquipmentQL)
--import Graphql.Asset.InventoryItem.Resolvers (getInventoryItemByIdResolver_)

workQueueResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (WorkQueues o)
workQueueResolver _ = pure WorkQueues { fetchPendingWorkQueues = fetchPedingWorkQueuesResolver
                                      , addWorkQueueDate = addWorkQueueDateResolver
                                      , addWorkQueueEvent = addWorkQueueEventResolver
                                      }

getWorkQueueByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkQueue_Id -> () -> o () Handler (WorkQueue o)
getWorkQueueByIdResolver_ workQueueId _ = lift $ do
                              workQueue <- runDB $ getJustEntity workQueueId
                              return $ toWorkQueueQL workQueue

fetchPedingWorkQueuesResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (WorkQueue o))
fetchPedingWorkQueuesResolver page = lift $ do
                        countItems <- fetchPendingWorkQueuesQueryCount page
                        queryResult <- fetchPendingWorkQueuesQuery page
                        let result = P.map (\ wq -> toWorkQueueQL wq) queryResult
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

workQueueByEquipmentIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Item_Id -> () -> o () Handler [WorkQueue o]
workQueueByEquipmentIdResolver_ equipmentId _ = lift $ do
                              workQueues <- fetchWorkQueueByEquipmentIdQuery equipmentId
                              let result = P.map (\ w -> toWorkQueueQL w) workQueues
                              return result

fetchWorkResourcesByWorkQueueIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkQueue_Id -> () -> o () Handler [WorkOrderResource o]
fetchWorkResourcesByWorkQueueIdResolver_ workQueueId _ = lift $ do
                              workOrderResources <-  runDB $ selectList [WorkOrderResource_WorkQueueId ==. workQueueId] []
                              return $ P.map (\ r -> toWorkOrderResourceQL r) workOrderResources

fetchWorkOrderSubTaskByWorkQueueIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkQueue_Id -> () -> o () Handler [WorkOrderSubTask o]
fetchWorkOrderSubTaskByWorkQueueIdResolver_ workQueueId _ = lift $ do
                              workOrderSubtask <-  runDB $ selectList [WorkOrderSubTask_WorkQueueId ==. workQueueId] []
                              return $ P.map (\ r -> toWorkOrderSubTaskQL r) workOrderSubtask

--fetchEquipmentsByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [Equipment o]
--fetchEquipmentsByWorkOrderIdResolver_ workOrderId _ = lift $ do
--                              workQueues <- fetchWorkQueuesByWorkOrderIdQuery workOrderId
--                              let result = P.map (\ (e, i) -> toEquipmentQL e i) workQueues
--                              return result

addWorkQueueDateResolver :: (MonadTrans t) => WorkQueueDateArg -> t Handler Bool
addWorkQueueDateResolver arg = lift $ do
                         workQueueSuccess <- addDateWorkQueuePersistent arg
                         return $ workQueueSuccess

addWorkQueueEventResolver :: (MonadTrans t) => WorkQueueEventArg -> t Handler Bool
addWorkQueueEventResolver arg = lift $ do
                         workQueueSuccess <- addEventWorkQueuePersistent arg
                         return $ workQueueSuccess


-- CONVERTERS
toWorkQueueQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) =>Entity WorkQueue_ -> WorkQueue o
toWorkQueueQL (Entity workQueueId workQueue) = WorkQueue { workQueueId = fromIntegral $ fromSqlKey workQueueId
                                                         , rescheduledDate = case workQueue_RescheduledDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , scheduledDate = fromString $ show workQueue_ScheduledDate
                                                         , incidentDate = case workQueue_IncidentDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , startWorkDate = case workQueue_StartWorkDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , finishedWorkDate = case workQueue_FinishedWorkDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , notes = workQueue_Notes
                                                         , outOfServiceInterval = workQueue_OutOfServiceInterval
                                                         , status = workQueue_Status
                                                         , workType = workQueue_WorkType
                                                         , equipment = getEquipmentByIdResolver_ workQueue_EquipmentId
                                                         , task = getTaskByIdResolver_ workQueue_TaskId
                                                         , taskTrigger = getTaskTriggerByIdResolver_ workQueue_TaskTriggerId
                                                         , workOrderResources = fetchWorkResourcesByWorkQueueIdResolver_ workQueueId
                                                         , workOrderSubTask = fetchWorkOrderSubTaskByWorkQueueIdResolver_ workQueueId
                                                         , createdDate = T.pack $ show workQueue_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            WorkQueue_ {..} = workQueue
                                            m = case workQueue_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

toWorkOrderResourceQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrderResource_ -> WorkOrderResource o
toWorkOrderResourceQL (Entity workOrderResourceId workOrderResource) = WorkOrderResource { workOrderResourceId = fromIntegral $ fromSqlKey workOrderResourceId
                                                                                         , amount = workOrderResource_Amount
                                                                                         , resourceType = workOrderResource_ResourceType
                                                                                         , employeeCategoryId = (case workOrderResource_EmployeeCategoryId of Nothing -> Nothing; Just a -> Just $ fromIntegral $ fromSqlKey a)
                                                                                         , itemId = (case workOrderResource_ItemId of Nothing -> Nothing; Just a -> Just $ fromIntegral $ fromSqlKey a)
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

toWorkOrderSubTaskQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrderSubTask_ -> WorkOrderSubTask o
toWorkOrderSubTaskQL (Entity workOrderSubTaskId workOrderSubTask) = WorkOrderSubTask { workOrderSubTaskId = fromIntegral $ fromSqlKey workOrderSubTaskId
                                                                                     , value = workOrderSubTask_Value
                                                                                     , subTask = getSubTaskByIdResolver_ workOrderSubTask_SubTaskId
                                                                                     , workQueue = getWorkQueueByIdResolver_ workOrderSubTask_WorkQueueId
                                                                                     , createdDate = fromString $ show workOrderSubTask_CreatedDate
                                                                                     , modifiedDate = m
                                                                                     }
                                                                        where
                                                                          WorkOrderSubTask_ {..} = workOrderSubTask
                                                                          m = case workOrderSubTask_ModifiedDate of
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
