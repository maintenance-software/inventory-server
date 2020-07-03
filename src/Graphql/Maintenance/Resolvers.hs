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
    , getWorkQueueByIdResolver_
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
--import Graphql.Admin.Person (getPersonByIdResolver_)
import Graphql.Maintenance.Task.DataTypes (Task(..))
import Graphql.Asset.Equipment.Resolvers (toEquipmentQL)
--import Graphql.Asset.InventoryItem.Resolvers (getInventoryItemByIdResolver_)

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
                              workQueues <- fetchWorkQueueByEquipmentIdQuery equipmentId
                              let result = P.map (\ w -> toWorkQueueQL w) workQueues
                              return result

--fetchEquipmentsByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [Equipment o]
--fetchEquipmentsByWorkOrderIdResolver_ workOrderId _ = lift $ do
--                              workQueues <- fetchWorkQueuesByWorkOrderIdQuery workOrderId
--                              let result = P.map (\ (e, i) -> toEquipmentQL e i) workQueues
--                              return result

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
                                                         , startWorkDate = case workQueue_StartWorkDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , finishedWorkDate = case workQueue_FinishedWorkDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                         , notes = workQueue_Notes
                                                         , outOfServiceInterval = workQueue_OutOfServiceInterval
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
