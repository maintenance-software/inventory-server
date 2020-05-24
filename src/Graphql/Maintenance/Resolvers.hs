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
                                          , taskActivities = taskActivityPageResolver
                                          , equipmentTasks = equipmentTasksResolver
                                          , addTaskActivityDate = addTaskActivityDateResolver
                                          , addTaskActivityEvent = addTaskActivityEventResolver
                                          , saveMaintenance = saveMaintenanceResolver
                                          , task = getTaskByIdResolver
                                          , createUpdateTasks = createUpdateTasksResolver
                                          , workOrder = getWorkOrderByIdResolver
                                          , createUpdateWorkOrder = createUpdateWorkOrderResolver
--                                          , eventTriggers = listEventTriggerResolver
--                                          , saveEventTrigger = saveEventTriggerResolver
                                          }

--getMaintenanceByIdResolver :: GetEntityByIdArg -> Res e Handler (Maintenance Res)
getMaintenanceByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => GetEntityByIdArg -> o () Handler (Maintenance o)
getMaintenanceByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let maintenanceId = (toSqlKey $ fromIntegral $ entityId)::Maintenance_Id
                                              maintenance <- runDB $ getJustEntity maintenanceId
                                              return $ toMaintenanceQL maintenance

getMaintenanceByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Maintenance_Id -> () -> o () Handler (Maintenance o)
getMaintenanceByIdResolver_ maintenanceId _ = lift $ do
                                    maintenance <- runDB $ getJustEntity maintenanceId
                                    return $ toMaintenanceQL maintenance

getWorkOrderByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => GetEntityByIdArg -> o () Handler (WorkOrder o)
getWorkOrderByIdResolver GetEntityByIdArg {..} = lift $ do
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

--taskActivityPageResolver :: PageArg -> t () Handler (Page TaskActivity)
taskActivityPageResolver page = lift $ do
                        countItems <- taskActivityQueryCount page
                        queryResult <- taskActivityQuery page
                        let result = P.map (\ (i, e, ta, t, tt, m) -> toTaskActivityQL i e ta t tt m) queryResult
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

equipmentTasksResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => GetEntityByIdArg -> t Handler [Task o]
equipmentTasksResolver GetEntityByIdArg {..} = lift $ do
                         let itemId = (toSqlKey $ fromIntegral $ entityId)::Item_Id
                         let equipmentKey = Equipment_Key {unEquipment_Key  = itemId}
                         Entity _ Equipment_{..} <- runDB $ getJustEntity equipmentKey
                         entityTasks <- case equipment_MaintenanceId of
                                    Nothing -> pure []
                                    Just maintenanceId -> taskQuery maintenanceId
                         return $ P.map (\t -> toTaskQL t) entityTasks

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

--addDateTaskActivityResolver :: TaskActivityDateArg -> t Handler Int
addTaskActivityDateResolver arg = lift $ do
                         taskActivitySuccess <- addDateTaskActivityPersistent arg
                         return $ taskActivitySuccess

--addDateTaskActivityResolver :: TaskActivityDateArg -> t Handler Int
addTaskActivityEventResolver arg = lift $ do
                         taskActivitySuccess <- addEventTaskActivityPersistent arg
                         return $ taskActivitySuccess

--addDateTaskActivityResolver :: TaskActivityDateArg -> t Handler Int
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

toTaskActivityQL :: Entity Item_ -> Entity Equipment_ -> Entity TaskActivity_ -> Entity Task_ -> Entity TaskTrigger_ -> Maybe (Entity Maintenance_) -> TaskActivity
toTaskActivityQL item equipment taskActivity task trigger maintenance = TaskActivity { taskActivityId = fromIntegral $ fromSqlKey taskActivityId
                                                                                     , scheduledDate = case taskActivity_ScheduledDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                                                     , calculatedDate = fromString $ show taskActivity_CalculatedDate
                                                                                     , rescheduled = taskActivity_Rescheduled
                                                                                     , incidentDate = case taskActivity_IncidentDate of Nothing -> Nothing; Just d -> Just $ fromString $ show d
                                                                                     , status = T.pack $ show taskActivity_Status
                                                                                     , assetId = fromIntegral $ fromSqlKey itemId
                                                                                     , assetCode = item_Code
                                                                                     , assetName = item_Name
                                                                                     , maintenanceId = maintenanceId
                                                                                     , maintenanceName = maintenanceName
                                                                                     , taskId = fromIntegral $ fromSqlKey taskId
                                                                                     , taskName = task_Name
                                                                                     , taskPriority = task_Priority
                                                                                     , taskTriggerId = fromIntegral $ fromSqlKey triggerId
                                                                                     , triggerDescription = taskTrigger_TriggerType
                                                                                     , taskType = taskActivity_TaskType
                                                                                     , createdDate = T.pack $ show taskActivity_CreatedDate
                                                                                     }
                                          where
                                            Entity itemId (Item_ {..}) = item
                                            Entity _ (Equipment_ {..}) = equipment
                                            Entity taskActivityId (TaskActivity_ {..}) = taskActivity
--                                            Entity maintenanceId (Maintenance_ {..}) = maintenance
                                            Entity taskId (Task_ {..}) = task
                                            Entity triggerId (TaskTrigger_ {..}) = trigger
                                            maintenanceName = (case maintenance of Nothing -> Nothing; Just (Entity _ (Maintenance_ {..})) -> Just maintenance_Name)
                                            maintenanceId = (case maintenance of Nothing -> Nothing; Just (Entity maintenanceId _) -> Just $ fromIntegral $ fromSqlKey maintenanceId)


--toWorkOrderQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrder_ -> WorkOrder o
toWorkOrderQL (Entity workOrderId workOrder) = WorkOrder { workOrderId = fromIntegral $ fromSqlKey workOrderId
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
