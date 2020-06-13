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

module Graphql.Maintenance.Persistence (
        createOrUpdateMaintenance
      , equipmentQuery
      , maintenanceQuery
      , maintenanceQueryCount
      , maintenanceFilters
      , availableEquipmentQuery
      , availableEquipmentQueryCount
      , workQueueQueryCount
      , workQueueQuery
      , addDateWorkQueuePersistent
      , addEventWorkQueuePersistent
      , createUpdateWorkOrderPersistent
      , workOrderQueryCount
      , workOrderQuery
      , workQueueCountTasksQuery
      , fetchPendingWorkQueueQueryCount
      , fetchPendingWorkQueueQuery
      , fetchPendingWorkQueueByEquipmentIdQuery
) where

import Import
import GHC.Generics
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Time.Calendar (toGregorian, fromGregorian)
--import Data.Text.Time (parseISODateTime)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_, countRows)
import Database.Esqueleto.PostgreSQL (arrayAggDistinct)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Data.Time
import Graphql.Maintenance.DataTypes
import Graphql.Asset.Equipment.Persistence (equipmentQueryFilters)
import Graphql.Maintenance.Task.Persistence (taskQuery)

getMaintenancePredicate maintenance Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                                   | T.strip field == "name" = [getOperator operator (maintenance ^. Maintenance_Name) (E.val value)]
                                                   | T.strip field == "status" = [getOperator operator (maintenance ^. Maintenance_Status) (E.val (readEntityStatus $ T.strip value))]
                                                   | otherwise = []

getMaintenanceInPredicate maintenance Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                                     | T.strip field == "name" = [(maintenance ^. Maintenance_Name) `in_` (E.valList $ fromText P.id value)]
                                                     | T.strip field == "status" = [(maintenance ^. Maintenance_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                                     | otherwise = []

getMaintenanceNotInPredicate maintenance Predicate {..} | T.strip operator /= "not in" || T.strip value == "" = []
                                                        | T.strip field == "name" = [(maintenance ^. Maintenance_Name) `notIn` (E.valList $ fromText P.id value)]
                                                        | T.strip field == "status" = [(maintenance ^. Maintenance_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                                        | otherwise = []
getMaintenancePredicates _ [] = []
getMaintenancePredicates maintenance (x:xs) | P.length p == 0 = getMaintenancePredicates maintenance xs
                                            | otherwise = p : getMaintenancePredicates maintenance xs
                   where
                      p = (getMaintenancePredicate maintenance x) P.++ (getMaintenanceInPredicate maintenance x) P.++ (getMaintenanceNotInPredicate maintenance x)

maintenanceFilters maintenance PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getMaintenancePredicates maintenance justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (maintenance ^. Maintenance_Id E.==. maintenance ^. Maintenance_Id)
                            let searchFilters = case searchString of
                                                  Just s -> [maintenance ^. Maintenance_Name `E.like` (%) ++. E.val s ++. (%)]
                                                  Nothing -> [maintenance ^. Maintenance_Id E.==. maintenance ^. Maintenance_Id]
                            let searchFilters_ = unionFilters searchFilters
                            return (searchFilters_ E.&&. predicates_)

workOrderPredicate workOrder Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                            | T.strip field == "workOrderCode" = [getOperator operator (workOrder ^. WorkOrder_WorkOrderCode) (E.val $ T.strip value)]
                                            | T.strip field == "workOrderStatus" = [getOperator operator (workOrder ^. WorkOrder_WorkOrderStatus) (E.val $ T.strip value)]
                                            | otherwise = []

workOrderInPredicate workOrder Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                              | T.strip field == "workOrderCode" = [(workOrder ^. WorkOrder_WorkOrderCode) `in_` (E.valList $ fromText P.id value)]
                                              | T.strip field == "workOrderStatus" = [(workOrder ^. WorkOrder_WorkOrderStatus) `in_` (E.valList $ fromText P.id value)]
                                              | otherwise = []

workOrderNotInPredicate workOrder Predicate {..} | T.strip operator /= "not in" || T.strip value == "" = []
                                                 | T.strip field == "workOrderCode" = [(workOrder ^. WorkOrder_WorkOrderCode) `notIn` (E.valList $ fromText P.id value)]
                                                 | T.strip field == "workOrderStatus" = [(workOrder ^. WorkOrder_WorkOrderStatus) `notIn` (E.valList $ fromText P.id value)]
                                                 | otherwise = []
workOrderPredicates _ [] = []
workOrderPredicates workOrder (x:xs) | P.length p == 0 = workOrderPredicates workOrder xs
                                     | otherwise = p : workOrderPredicates workOrder xs
                   where
                      p = (workOrderPredicate workOrder x) P.++ (workOrderInPredicate workOrder x) P.++ (workOrderNotInPredicate workOrder x)

workOrderFilters workOrder PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ workOrderPredicates workOrder justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (workOrder ^. WorkOrder_Id E.==. workOrder ^. WorkOrder_Id)
                            return predicates_

maintenanceQueryCount :: PageArg -> Handler Int
maintenanceQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \ maintenance -> do
                                        filters <- maintenanceFilters maintenance page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

maintenanceQuery :: PageArg -> Handler [Entity Maintenance_]
maintenanceQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ maintenance -> do
                                        filters <- maintenanceFilters maintenance page
                                        E.where_ filters
                                        E.orderBy [E.asc (maintenance ^. Maintenance_Id)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return maintenance
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

equipmentQuery :: Maintenance_Id -> Handler [(Entity Equipment_, Entity Item_)]
equipmentQuery maintenanceId =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
--                                        let subquery =
--                                              E.from $ \workQueue -> do
--                                              E.where_ (workQueue ^. WorkQueue_MaintenanceId E.==. E.val (Just maintenanceId))
--                                              return (workQueue ^. WorkQueue_EquipmentId)
--                                        E.where_ (equipment ^. Equipment_ItemId `E.in_` E.subList_select subquery)
                                        E.where_ (equipment ^. Equipment_MaintenanceId E.==. E.val (Just maintenanceId))
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        return (equipment, item)
                      return result

availableEquipmentQueryCount :: PageArg -> Handler Int
availableEquipmentQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
--                                        let subquery =
--                                              E.from $ \workQueue -> do
--                                              E.where_ (workQueue ^. WorkQueue_Status E.!=. E.val DELETED)
--                                              return (workQueue ^. WorkQueue_EquipmentId)
                                        filters <- equipmentQueryFilters equipment item page
--                                        E.where_ (filters E.&&. equipment ^. Equipment_ItemId `E.notIn` E.subList_select subquery)
                                        E.where_ (filters E.&&. E.isNothing (equipment ^. Equipment_MaintenanceId))
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

availableEquipmentQuery :: PageArg -> Handler [(Entity Equipment_, Entity Item_)]
availableEquipmentQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        let subquery =
                                              E.from $ \workQueue -> do
                                              E.where_ (workQueue ^. WorkQueue_Status E.!=. E.val DELETED)
                                              return (workQueue ^. WorkQueue_EquipmentId)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ (filters E.&&. equipment ^. Equipment_ItemId `E.notIn` E.subList_select subquery)
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return (equipment, item)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

workQueueQueryCount :: PageArg -> Handler Int
workQueueQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(item `E.InnerJoin` equipment `E.InnerJoin` workQueue `E.InnerJoin` task `E.InnerJoin` trigger `E.LeftOuterJoin` maintenance `E.LeftOuterJoin` category) -> do
                                        E.on $ item ^. Item_Id E.==. equipment ^. Equipment_ItemId
                                        E.on $ equipment ^. Equipment_ItemId E.==. workQueue ^. WorkQueue_EquipmentId
                                        E.on $ workQueue ^. WorkQueue_TaskId E.==. task ^. Task_Id
                                        E.on $ workQueue ^. WorkQueue_TaskTriggerId E.==. trigger ^. TaskTrigger_Id
                                        E.on $ (workQueue ^. WorkQueue_MaintenanceId) E.==. (maintenance ?. Maintenance_Id)
                                        E.on $ (task ^. Task_TaskCategoryId) E.==. (category ?. Category_Id)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

workQueueQuery :: PageArg -> Handler [(Entity Item_, Entity Equipment_, Entity WorkQueue_, Entity Task_, Entity TaskTrigger_, Maybe (Entity Maintenance_), Maybe (Entity Category_))]
workQueueQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(item `E.InnerJoin` equipment `E.InnerJoin` workQueue `E.InnerJoin` task `E.InnerJoin` trigger `E.LeftOuterJoin` maintenance `E.LeftOuterJoin` category) -> do
                                        E.on $ item ^. Item_Id E.==. equipment ^. Equipment_ItemId
                                        E.on $ equipment ^. Equipment_ItemId E.==. workQueue ^. WorkQueue_EquipmentId
                                        E.on $ workQueue ^. WorkQueue_TaskId E.==. task ^. Task_Id
                                        E.on $ workQueue ^. WorkQueue_TaskTriggerId E.==. trigger ^. TaskTrigger_Id
                                        E.on $ (workQueue ^. WorkQueue_MaintenanceId) E.==. (maintenance ?. Maintenance_Id)
                                        E.on $ (task ^. Task_TaskCategoryId) E.==. (category ?. Category_Id)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ filters
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return (item, equipment, workQueue, task, trigger, maintenance, category)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10


fetchPendingWorkQueueQueryCount :: PageArg -> Handler Int
fetchPendingWorkQueueQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(item `E.InnerJoin` equipment) -> do
                                        E.on $ item ^. Item_Id E.==. equipment ^. Equipment_ItemId
                                        let subquery = E.from $ \workQueue -> do
                                                       E.where_ (workQueue ^. WorkQueue_Status E.==. (E.val $ PENDING))
                                                       return (workQueue ^. WorkQueue_EquipmentId)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ (filters E.&&. equipment ^. Equipment_ItemId `E.in_` E.subList_select subquery)
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

fetchPendingWorkQueueQuery :: PageArg -> Handler [(Entity Equipment_, Entity Item_)]
fetchPendingWorkQueueQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(item `E.InnerJoin` equipment) -> do
                                        E.on $ item ^. Item_Id E.==. equipment ^. Equipment_ItemId
                                        let subquery = E.from $ \workQueue -> do
                                                       E.where_ (workQueue ^. WorkQueue_Status E.==. (E.val $ PENDING))
                                                       return (workQueue ^. WorkQueue_EquipmentId)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ (filters E.&&. equipment ^. Equipment_ItemId `E.in_` E.subList_select subquery)
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return (equipment, item)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

fetchPendingWorkQueueByEquipmentIdQuery :: Item_Id -> Handler [Entity WorkQueue_]
fetchPendingWorkQueueByEquipmentIdQuery equipmentId =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ workQueue -> do
                                        E.where_ (workQueue ^. WorkQueue_Status E.==. (E.val $ PENDING) E.&&. workQueue ^. WorkQueue_EquipmentId E.==. (E.val $ equipmentId))
                                        E.orderBy [E.asc (workQueue ^. WorkQueue_Id)]
                                        return workQueue
                      return result

--createOrUpdateMaintenance :: MaintenanceArg -> Handler (Maintenance MutRes)
createOrUpdateMaintenance maintenance = do
                let MaintenanceArg {..} = maintenance
                now <- liftIO getCurrentTime
                entityId <- if maintenanceId > 0 then
                                do
                                  let maintenanceKey = (toSqlKey $ fromIntegral $ maintenanceId)::Maintenance_Id
                                  _ <- runDB $ update maintenanceKey [ Maintenance_Name =. name
                                                                     , Maintenance_Description =. description
                                                                     , Maintenance_Status =. readEntityStatus status
                                                                     , Maintenance_ModifiedDate =. Just now
                                                                     ]
                                  return maintenanceKey
                               else do
                                  maintenanceKey <- runDB $ insert $ fromMaintenanceQL maintenance now Nothing
                                  return maintenanceKey
                return entityId

addEventWorkQueuePersistent :: WorkQueueEventArg -> Handler Bool
addEventWorkQueuePersistent WorkQueueEventArg {..} = do
                    now <- liftIO getCurrentTime
                    let assetEntityId = ((toSqlKey $ fromIntegral $ assetId)::Item_Id)
                    let taskEntityId = ((toSqlKey $ fromIntegral $ taskId)::Task_Id)
                    let maintenanceEntityId = case maintenanceId of Nothing -> Nothing; Just mid -> Just ((toSqlKey $ fromIntegral $ mid)::Maintenance_Id)
                    let reportedByEntityId = ((toSqlKey $ fromIntegral $ reportedById)::Person_Id)
                    let taskTriggerEntityId = ((toSqlKey $ fromIntegral $ taskTriggerId)::TaskTrigger_Id)
                    let incidentUtcDate = case incidentDate of Nothing -> Nothing; Just d -> Just ((read $ T.unpack d)::UTCTime)
                    let newWorkQueue = WorkQueue_ { workQueue_RescheduledDate = Nothing
                                                     , workQueue_ScheduledDate = now
                                                     , workQueue_IncidentDate = incidentUtcDate
                                                     , workQueue_TaskId = taskEntityId
                                                     , workQueue_TaskTriggerId = taskTriggerEntityId
                                                     , workQueue_Status = PENDING
                                                     , workQueue_WorkType = if hasAssetFailure then "EVENT_FAILURE" else "EVENT"
                                                     , workQueue_MaintenanceId = maintenanceEntityId
                                                     , workQueue_EquipmentId = assetEntityId
                                                     , workQueue_WorkOrderId = Nothing
                                                     , workQueue_ReportedById = Just reportedByEntityId
                                                     , workQueue_ModifiedDate = Nothing
                                                     , workQueue_CreatedDate = now
                                                     }
                    _ <- runDB $ insert $ newWorkQueue
--                    let equipmentKey = Equipment_Key {unEquipment_Key  = assetEntityId}
--                    _ <- runDB $ update equipmentKey [ Equipment_MaintenanceId =. maintenanceEntityId, Equipment_ModifiedDate =. Just now ]
                    return True

addDateWorkQueuePersistent :: WorkQueueDateArg -> Handler Bool
addDateWorkQueuePersistent WorkQueueDateArg {..} = do
                    now <- liftIO getCurrentTime
                    let maintenanceUtcDate = (read $ T.unpack lastMaintenanceDate)::UTCTime
                    let maintenanceEntityId = ((toSqlKey $ fromIntegral $ maintenanceId)::Maintenance_Id)
                    let assetEntityId = ((toSqlKey $ fromIntegral $ assetId)::Item_Id)
                    tasks <- taskQuery maintenanceEntityId
                    let taskIds = P.map (\(Entity taskId _) -> taskId) tasks
                    triggers <-  runDB $ selectList [TaskTrigger_TaskId <-. taskIds, TaskTrigger_TriggerType ==. "DATE"] []
                    _ <- createWorkQueueForDate maintenanceEntityId assetEntityId maintenanceUtcDate triggers
--                    let equipmentKey = Equipment_Key {unEquipment_Key  = assetEntityId}
--                    _ <- runDB $ update equipmentKey [ Equipment_MaintenanceId =. Just maintenanceEntityId, Equipment_ModifiedDate =. Just now ]
                    return True

createWorkQueueForDate :: Maintenance_Id -> Item_Id -> UTCTime -> [Entity TaskTrigger_] -> Handler [WorkQueue_Id]
createWorkQueueForDate _ _ _ []  = pure []
createWorkQueueForDate maintenanceId assetId maintenanceUtcDate (h:hs) = do
                    now <- liftIO getCurrentTime
                    let UTCTime today _ = now
                    let (a, b, _) = toWeekDate today
                    let (x, y, _) = toGregorian today
                    let firstDayOfCurrentWeek = fromWeekDate a b 1
                    let firstDayOfCurrentMonth = fromGregorian x y 1
                    let firstDayOfCurrentYear = fromGregorian x 1 1
                    let Entity taskTriggerId (TaskTrigger_ {..}) = h
                    unitKey <- case taskTrigger_UnitId of
                                Nothing -> pure DAY
                                Just unitId -> do
                                                Entity _ Unit_ {..} <- runDB $ getJustEntity unitId
                                                return $ readTimeFrequency unit_Key
                    let calculatedDate = case unitKey of
                                          DAY -> today
                                          WEEK -> firstDayOfCurrentWeek
                                          MONTH -> firstDayOfCurrentMonth
                                          YEAR -> firstDayOfCurrentYear
                    let newWorkQueue = WorkQueue_ { workQueue_RescheduledDate = Nothing
                                                     , workQueue_ScheduledDate = UTCTime calculatedDate 0
                                                     , workQueue_IncidentDate = Nothing
                                                     , workQueue_TaskId = taskTrigger_TaskId
                                                     , workQueue_TaskTriggerId = taskTriggerId
                                                     , workQueue_Status = PENDING
                                                     , workQueue_WorkType = "PLAN"
                                                     , workQueue_MaintenanceId = (Just maintenanceId)
                                                     , workQueue_EquipmentId = assetId
                                                     , workQueue_WorkOrderId = Nothing
                                                     , workQueue_ReportedById = Nothing
                                                     , workQueue_ModifiedDate = Nothing
                                                     , workQueue_CreatedDate = now
                                                     }
                    workQueueEntityId <- runDB $ insert $ newWorkQueue
                    workQueueEntityIds <- createWorkQueueForDate maintenanceId assetId maintenanceUtcDate hs
                    return (workQueueEntityId:workQueueEntityIds)


--workQueueCountTasksQuery :: WoResourceRequirement -> Handler [(Item_Id, Task_Id, Int)]
--workQueueCountTasksQuery WoResourceRequirement{..} =  do
--                      result <- runDB
--                                   $ E.select
--                                   $ E.from $ \ workQueue -> do
--                                        E.where_ (workQueue ^. WorkQueue_Id `in_` (E.valList $ P.map (\i -> toSqlKey $ fromIntegral i) workQueueIds))
--                                        E.groupBy (workQueue ^. WorkQueue_EquipmentId, workQueue ^. WorkQueue_TaskId)
--                                        let count' = E.count (workQueue ^. WorkQueue_TaskId)
--                                        return (workQueue ^. WorkQueue_EquipmentId, workQueue ^. WorkQueue_TaskId, count')
--                      return $ fmap (\(E.Value a, E.Value b, E.Value c) -> (a, b, c)) $ result

workQueueCountTasksQuery :: EntityIdsArg -> Handler [(Item_Id, [Task_Id])]
workQueueCountTasksQuery EntityIdsArg{..} =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ workQueue -> do
                                        E.where_ (workQueue ^. WorkQueue_Id `in_` (E.valList $ P.map (\i -> toSqlKey $ fromIntegral i) entityIds))
                                        E.groupBy (workQueue ^. WorkQueue_EquipmentId)
                                        let toList = arrayAggDistinct (workQueue ^. WorkQueue_TaskId)
                                        return (workQueue ^. WorkQueue_EquipmentId, toList)
                      return $ fmap (\(E.Value a, E.Value (Just b)) -> (a, b)) $ result


workOrderQueryCount :: PageArg -> Handler Int
workOrderQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \ workOrder -> do
                                        filters <- workOrderFilters workOrder page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

workOrderQuery :: PageArg -> Handler [Entity WorkOrder_]
workOrderQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ workOrder -> do
                                        filters <- workOrderFilters workOrder page
                                        E.where_ filters
                                        E.orderBy [E.asc (workOrder ^. WorkOrder_Id)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return workOrder
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

createUpdateWorkOrderPersistent :: WorkOrderArg -> Handler WorkOrder_Id
createUpdateWorkOrderPersistent arg = do
                let WorkOrderArg {..} = arg
                now <- liftIO getCurrentTime
                randomCode <- liftIO $ randomAlphaNumText 6
                entityId <- if workOrderId > 0 then
                                do
                                  let workOrderKey = (toSqlKey $ fromIntegral $ workOrderId)::WorkOrder_Id
                                  _ <- runDB $ update workOrderKey [ WorkOrder_WorkOrderStatus =. workOrderStatus
                                                                   , WorkOrder_EstimateDuration =. estimateDuration
                                                                   , WorkOrder_Rate =. rate
                                                                   , WorkOrder_Notes =. notes
                                                                   , WorkOrder_GeneratedById =. ((toSqlKey $ fromIntegral $ generatedById)::Person_Id)
                                                                   , WorkOrder_ResponsibleId =. ((toSqlKey $ fromIntegral $ responsibleId)::Person_Id)
                                                                   , WorkOrder_ParentId =. (case parentId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::WorkOrder_Id))
                                                                   , WorkOrder_ModifiedDate =. Just now
                                                                   ]
                                  return workOrderKey
                               else do
                                  workOrderKey <- runDB $ insert $ fromWorkOrderQL arg now Nothing randomCode
                                  return workOrderKey
                _ <- saveWorkOrderResource ((toSqlKey $ fromIntegral $ workOrderId)::WorkOrder_Id) resources
                return entityId

saveWorkOrderResource :: WorkOrder_Id -> [WorkOrderResourceArg] -> Handler [WorkOrderResource_Id]
saveWorkOrderResource _ [] = pure []
saveWorkOrderResource workOrderId (x:xs) = do
                                  resourceId <-  createUpdateWorkOrderResource workOrderId x
                                  resourceIds <- saveWorkOrderResource workOrderId xs
                                  return (resourceId:resourceIds)

createUpdateWorkOrderResource :: WorkOrder_Id -> WorkOrderResourceArg -> Handler WorkOrderResource_Id
createUpdateWorkOrderResource workOrderId resource = do
                let WorkOrderResourceArg {..} = resource
                now <- liftIO getCurrentTime
                entityId <- if workOrderResourceId > 0 then
                                do
                                  let workOrderResourceKey = (toSqlKey $ fromIntegral $ workOrderResourceId)::WorkOrderResource_Id
                                  _ <- runDB $ update workOrderResourceKey [ WorkOrderResource_HumanResourceId =. (case humanResourceId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::Person_Id))
                                                                           , WorkOrderResource_Amount =. amount
                                                                           , WorkOrderResource_InventoryItemId =. (case inventoryItemId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::InventoryItem_Id))
                                                                           , WorkOrderResource_WorkOrderId =. workOrderId
                                                                           , WorkOrderResource_WorkQueueId =. ((toSqlKey $ fromIntegral $ workQueueId)::WorkQueue_Id)
                                                                           , WorkOrderResource_ModifiedDate =. Just now
                                                                          ]
                                  return workOrderResourceKey
                               else do
                                  workOrderResourceKey <- runDB $ insert $ fromWorkOrderResourceQL workOrderId resource now Nothing
                                  return workOrderResourceKey
                return entityId

fromMaintenanceQL :: MaintenanceArg -> UTCTime -> Maybe UTCTime -> Maintenance_
fromMaintenanceQL (MaintenanceArg {..}) cd md = Maintenance_ { maintenance_Name = name
                                                             , maintenance_Description = description
                                                             , maintenance_Status = readEntityStatus status
                                                             , maintenance_CreatedDate = cd
                                                             , maintenance_ModifiedDate = md
                                                             }

fromWorkOrderQL :: WorkOrderArg -> UTCTime -> Maybe UTCTime -> Text -> WorkOrder_
fromWorkOrderQL (WorkOrderArg {..}) cd md code = WorkOrder_ { workOrder_WorkOrderCode = code
                                                            , workOrder_WorkOrderStatus = workOrderStatus
                                                            , workOrder_EstimateDuration = estimateDuration
                                                            , workOrder_ExecutionDuration = 0
                                                            , workOrder_Rate = rate
                                                            , workOrder_TotalCost = 0
                                                            , workOrder_Percentage = 0
                                                            , workOrder_Notes = notes
                                                            , workOrder_GeneratedById = ((toSqlKey $ fromIntegral $ generatedById)::Person_Id)
                                                            , workOrder_ResponsibleId = ((toSqlKey $ fromIntegral $ responsibleId)::Person_Id)
                                                            , workOrder_CanceledById = Nothing
                                                            , workOrder_ParentId = (case parentId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::WorkOrder_Id))
                                                            , workOrder_CloseDate = Nothing
                                                            , workOrder_CreatedDate = cd
                                                            , workOrder_ModifiedDate = md
                                                            }

fromWorkOrderResourceQL :: WorkOrder_Id -> WorkOrderResourceArg -> UTCTime -> Maybe UTCTime -> WorkOrderResource_
fromWorkOrderResourceQL workOrderId (WorkOrderResourceArg {..}) cd md = WorkOrderResource_ { workOrderResource_HumanResourceId = (case humanResourceId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::Person_Id))
                                                                                           , workOrderResource_Amount = amount
                                                                                           , workOrderResource_InventoryItemId =  (case inventoryItemId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::InventoryItem_Id))
                                                                                           , workOrderResource_WorkQueueId = (toSqlKey $ fromIntegral workQueueId)::WorkQueue_Id
                                                                                           , workOrderResource_WorkOrderId = workOrderId
                                                                                           , workOrderResource_CreatedDate = cd
                                                                                           , workOrderResource_ModifiedDate = md
                                                                                           }
