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
) where

import Import
import Data.Morpheus.Types (lift)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums ()
import Graphql.Utils
import Graphql.Maintenance.Task.Resolvers
import Graphql.Maintenance.Task.Persistence
import Graphql.Asset.Equipment.Resolvers
import Graphql.Maintenance.DataTypes
import Graphql.Maintenance.Persistence
import Graphql.Category
import Graphql.Admin.Person (getPersonByIdResolver_)
import Graphql.Maintenance.Task.DataTypes (Task(..))
import Graphql.Asset.Equipment.DataTypes (Equipment(..))
--maintenanceResolver :: () -> Res e Handler Maintenances
maintenanceResolver _ = pure Maintenances { maintenance = getMaintenanceByIdResolver
                                          , page = maintenancePageResolver
                                          , availableEquipments = availableEquipmentPageResolver
                                          , taskActivities = workQueuePageResolver
                                          , equipmentTasks = equipmentTasksResolver
                                          , addWorkQueueDate = addWorkQueueDateResolver
                                          , addWorkQueueEvent = addWorkQueueEventResolver
                                          , saveMaintenance = saveMaintenanceResolver
                                          , task = getTaskByIdResolver
                                          , createUpdateTasks = createUpdateTasksResolver
                                          , workOrder = getWorkOrderByIdResolver
                                          , createUpdateWorkOrder = createUpdateWorkOrderResolver
                                          , workOrders = workOrderPageResolver
                                          , woPreResources = woPreResourcesResolver
--                                          , eventTriggers = listEventTriggerResolver
--                                          , saveEventTrigger = saveEventTriggerResolver
                                          }

--getMaintenanceByIdResolver :: EntityIdArg -> Res e Handler (Maintenance Res)
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

--workQueuePageResolver :: PageArg -> t () Handler (Page WorkQueue)
workQueuePageResolver page = lift $ do
                        countItems <- workQueueQueryCount page
                        queryResult <- workQueueQuery page
                        let result = P.map (\ (i, e, ta, t, tt, m, c) -> toWorkQueueQL i e ta t tt m c) queryResult
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

--workQueuePageResolver :: PageArg -> t () Handler (Page WorkQueue)
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


equipmentResolver_ :: (MonadTrans t, MonadTrans (o ())) => Maintenance_Id -> p -> t Handler [Equipment o]
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



--resourceRequirementResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => WoResourceRequirement -> t Handler [WoAssets]
woPreResourcesResolver requestArg = lift $ do
                       equipmentIds <- workQueueCountTasksQuery requestArg
                       equipments <- runDB $ selectList [Item_Id <-. (P.map (\(a, _) -> a) equipmentIds)] []
                       tasks <- runDB $ selectList [Task_Id <-. (P.concat $ P.map (\(_, b) -> b) equipmentIds)] []
                       let findAssetById = \assetId -> (P.filter (\(Entity i _) -> i == assetId) equipments)
                       let findTasksByIds = \taskIds -> (P.filter (\ (Entity i _) -> i `P.elem` taskIds) tasks)
                       let response = P.map (\(itemId, taskIds) -> (toWOAsset (P.head $ findAssetById itemId) (findTasksByIds taskIds))) equipmentIds
                       return response

toWOAsset :: Entity Item_ -> [Entity Task_] -> WoAssets
toWOAsset (Entity itemId Item_{..}) tasks = WoAssets { assetId = fromIntegral $ fromSqlKey itemId
                                                     , name = item_Name
                                                     , tasks = P.map toWOTask tasks
                                                     }

toWOTask :: Entity Task_ -> WoAssetTask
toWOTask (Entity taskId Task_{..}) = WoAssetTask { taskId = fromIntegral $ fromSqlKey taskId
                                                 , name = task_Name
                                                 , requiredResource = True
                                                 }



--saveMaintenanceResolver :: MaintenanceArg -> MutRes e Handler (Maintenance MutRes)
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

--addDateWorkQueueResolver :: WorkQueueDateArg -> t Handler Int
addWorkQueueDateResolver arg = lift $ do
                         workQueueSuccess <- addDateWorkQueuePersistent arg
                         return $ workQueueSuccess

--addDateWorkQueueResolver :: WorkQueueDateArg -> t Handler Int
addWorkQueueEventResolver arg = lift $ do
                         workQueueSuccess <- addEventWorkQueuePersistent arg
                         return $ workQueueSuccess

--addDateWorkQueueResolver :: WorkQueueDateArg -> t Handler Int
createUpdateWorkOrderResolver arg = lift $ do
                         workOrderId <- createUpdateWorkOrderPersistent arg
                         workOrder <-  runDB $ getJustEntity workOrderId
                         return $ toWorkOrderQL workOrder

-- CONVERTERS
--toMaintenanceQL :: Entity Maintenance_ -> Maintenance
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

toWorkQueueQL :: Entity Item_ -> Entity Equipment_ -> Entity WorkQueue_ -> Entity Task_ -> Entity TaskTrigger_ -> Maybe (Entity Maintenance_) -> Maybe (Entity Category_) -> WorkQueue
toWorkQueueQL item equipment workQueue task trigger maintenance category = WorkQueue { workQueueId = fromIntegral $ fromSqlKey workQueueId
                                                                                     , rescheduledDate = case workQueue_RescheduledDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                                                     , scheduledDate = fromString $ show workQueue_ScheduledDate
                                                                                     , incidentDate = case workQueue_IncidentDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                                                     , status = T.pack $ show workQueue_Status
                                                                                     , assetId = fromIntegral $ fromSqlKey itemId
                                                                                     , assetCode = item_Code
                                                                                     , assetName = item_Name
                                                                                     , maintenanceId = maintenanceId
                                                                                     , maintenanceName = maintenanceName
                                                                                     , taskId = fromIntegral $ fromSqlKey taskId
                                                                                     , taskName = task_Name
                                                                                     , taskPriority = task_Priority
                                                                                     , taskCategoryId = categoryId
                                                                                     , taskCategoryName = categoryName
                                                                                     , taskTriggerId = fromIntegral $ fromSqlKey triggerId
                                                                                     , triggerDescription = taskTrigger_TriggerType
                                                                                     , workType = workQueue_WorkType
                                                                                     , createdDate = T.pack $ show workQueue_CreatedDate
                                                                                     }
                                          where
                                            Entity itemId (Item_ {..}) = item
                                            Entity _ (Equipment_ {..}) = equipment
                                            Entity workQueueId (WorkQueue_ {..}) = workQueue
--                                            Entity maintenanceId (Maintenance_ {..}) = maintenance
                                            Entity taskId (Task_ {..}) = task
                                            Entity triggerId (TaskTrigger_ {..}) = trigger
                                            maintenanceName = (case maintenance of Nothing -> Nothing; Just (Entity _ (Maintenance_ {..})) -> Just maintenance_Name)
                                            maintenanceId = (case maintenance of Nothing -> Nothing; Just (Entity maintenanceId _) -> Just $ fromIntegral $ fromSqlKey maintenanceId)
                                            categoryName = (case category of Nothing -> Nothing; Just (Entity _ (Category_ {..})) -> Just category_Name)
                                            categoryId = (case category of Nothing -> Nothing; Just (Entity categoryId _) -> Just $ fromIntegral $ fromSqlKey categoryId)

--toWorkOrderQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrder_ -> WorkOrder o
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
                                                         , createdDate = fromString $ show workOrder_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            WorkOrder_ {..} = workOrder
                                            m = case workOrder_ModifiedDate of
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
