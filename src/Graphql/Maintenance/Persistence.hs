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
      , taskActivityQueryCount
      , taskActivityQuery
      , addDateTaskActivityPersistent
      , addEventTaskActivityPersistent
) where

import Import
import GHC.Generics
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Time.Calendar (toGregorian, fromGregorian)
--import Data.Text.Time (parseISODateTime)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
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
                                        let subquery =
                                              E.from $ \taskActivity -> do
                                              E.where_ (taskActivity ^. TaskActivity_MaintenanceId E.==. E.val (Just maintenanceId))
                                              return (taskActivity ^. TaskActivity_EquipmentId)
                                        E.where_ (equipment ^. Equipment_ItemId `E.in_` E.subList_select subquery)
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        return (equipment, item)
                      return result

availableEquipmentQueryCount :: PageArg -> Handler Int
availableEquipmentQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        let subquery =
                                              E.from $ \taskActivity -> do
                                              E.where_ (taskActivity ^. TaskActivity_Status E.!=. E.val DELETED)
                                              return (taskActivity ^. TaskActivity_EquipmentId)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ (filters E.&&. equipment ^. Equipment_ItemId `E.notIn` E.subList_select subquery)
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

availableEquipmentQuery :: PageArg -> Handler [(Entity Equipment_, Entity Item_)]
availableEquipmentQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        let subquery =
                                              E.from $ \taskActivity -> do
                                              E.where_ (taskActivity ^. TaskActivity_Status E.!=. E.val DELETED)
                                              return (taskActivity ^. TaskActivity_EquipmentId)
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

taskActivityQueryCount :: PageArg -> Handler Int
taskActivityQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(item `E.InnerJoin` equipment `E.InnerJoin` taskActivity `E.InnerJoin` task `E.InnerJoin` trigger` E.LeftOuterJoin` maintenance) -> do
                                        E.on $ item ^. Item_Id E.==. equipment ^. Equipment_ItemId
                                        E.on $ equipment ^. Equipment_ItemId E.==. taskActivity ^. TaskActivity_EquipmentId
                                        E.on $ taskActivity ^. TaskActivity_TaskId E.==. task ^. Task_Id
                                        E.on $ taskActivity ^. TaskActivity_TaskTriggerId E.==. trigger ^. TaskTrigger_Id
                                        E.on $ (taskActivity ^. TaskActivity_MaintenanceId) E.==. (maintenance ?. Maintenance_Id)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

taskActivityQuery :: PageArg -> Handler [(Entity Item_, Entity Equipment_, Entity TaskActivity_, Entity Task_, Entity TaskTrigger_, Maybe (Entity Maintenance_))]
taskActivityQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(item `E.InnerJoin` equipment `E.InnerJoin` taskActivity `E.InnerJoin` task `E.InnerJoin` trigger` E.LeftOuterJoin` maintenance) -> do
                                        E.on $ item ^. Item_Id E.==. equipment ^. Equipment_ItemId
                                        E.on $ equipment ^. Equipment_ItemId E.==. taskActivity ^. TaskActivity_EquipmentId
                                        E.on $ taskActivity ^. TaskActivity_TaskId E.==. task ^. Task_Id
                                        E.on $ taskActivity ^. TaskActivity_TaskTriggerId E.==. trigger ^. TaskTrigger_Id
                                        E.on $ (taskActivity ^. TaskActivity_MaintenanceId) E.==. (maintenance ?. Maintenance_Id)
                                        filters <- equipmentQueryFilters equipment item page
                                        E.where_ filters
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return (item, equipment, taskActivity, task, trigger, maintenance)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

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

addEventTaskActivityPersistent :: TaskActivityEventArg -> Handler Bool
addEventTaskActivityPersistent TaskActivityEventArg {..} = do
                    now <- liftIO getCurrentTime
                    let assetEntityId = ((toSqlKey $ fromIntegral $ assetId)::Item_Id)
                    let taskEntityId = ((toSqlKey $ fromIntegral $ taskTriggerId)::Task_Id)
                    let maintenanceEntityId = case maintenanceId of Nothing -> Nothing; Just mid -> Just ((toSqlKey $ fromIntegral $ mid)::Maintenance_Id)
                    let reportedByEntityId = ((toSqlKey $ fromIntegral $ reportedById)::Person_Id)
                    let taskTriggerEntityId = ((toSqlKey $ fromIntegral $ taskTriggerId)::TaskTrigger_Id)
                    let incidentUtcDate = case incidentDate of Nothing -> Nothing; Just d -> Just ((read $ T.unpack d)::UTCTime)
                    let newTaskActivity = TaskActivity_ { taskActivity_ScheduledDate = Nothing
                                                        , taskActivity_CalculatedDate = now
                                                        , taskActivity_Rescheduled = False
                                                        , taskActivity_IncidentDate = incidentUtcDate
                                                        , taskActivity_TaskId = taskEntityId
                                                        , taskActivity_TaskTriggerId = taskTriggerEntityId
                                                        , taskActivity_Status = ACTIVE
                                                        , taskActivity_TaskType = if hasAssetFailure then "EVENT_FAILURE" else "EVENT"
                                                        , taskActivity_MaintenanceId = maintenanceEntityId
                                                        , taskActivity_EquipmentId = assetEntityId
                                                        , taskActivity_WorkOrderId = Nothing
                                                        , taskActivity_ReportedById = Just reportedByEntityId
                                                        , taskActivity_ModifiedDate = Nothing
                                                        , taskActivity_CreatedDate = now
                                                        }
                    _ <- runDB $ insert $ newTaskActivity
                    return True

addDateTaskActivityPersistent :: TaskActivityDateArg -> Handler Bool
addDateTaskActivityPersistent TaskActivityDateArg {..} = do
                    let maintenanceUtcDate = (read $ T.unpack lastMaintenanceDate)::UTCTime
                    let maintenanceEntityId = ((toSqlKey $ fromIntegral $ maintenanceId)::Maintenance_Id)
                    let assetEntityId = ((toSqlKey $ fromIntegral $ assetId)::Item_Id)
                    tasks <- taskQuery maintenanceEntityId
                    let taskIds = P.map (\(Entity taskId _) -> taskId) tasks
                    triggers <-  runDB $ selectList [TaskTrigger_TaskId <-. taskIds, TaskTrigger_TriggerType ==. "DATE"] []
                    _ <- createTaskActivityForDate maintenanceEntityId assetEntityId maintenanceUtcDate triggers
                    return True

createTaskActivityForDate :: Maintenance_Id -> Item_Id -> UTCTime -> [Entity TaskTrigger_] -> Handler [TaskActivity_Id]
createTaskActivityForDate _ _ _ []  = pure []
createTaskActivityForDate maintenanceId assetId maintenanceUtcDate (h:hs) = do
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
                    let newTaskActivity = TaskActivity_ { taskActivity_ScheduledDate = Nothing
                                                        , taskActivity_CalculatedDate = UTCTime calculatedDate 0
                                                        , taskActivity_Rescheduled = False
                                                        , taskActivity_IncidentDate = Nothing
                                                        , taskActivity_TaskId = taskTrigger_TaskId
                                                        , taskActivity_TaskTriggerId = taskTriggerId
                                                        , taskActivity_Status = ACTIVE
                                                        , taskActivity_TaskType = "PLAN"
                                                        , taskActivity_MaintenanceId = (Just maintenanceId)
                                                        , taskActivity_EquipmentId = assetId
                                                        , taskActivity_WorkOrderId = Nothing
                                                        , taskActivity_ReportedById = Nothing
                                                        , taskActivity_ModifiedDate = Nothing
                                                        , taskActivity_CreatedDate = now
                                                        }
                    taskActivityEntityId <- runDB $ insert $ newTaskActivity
                    taskActivityEntityIds <- createTaskActivityForDate maintenanceId assetId maintenanceUtcDate hs
                    return (taskActivityEntityId:taskActivityEntityIds)

fromMaintenanceQL :: MaintenanceArg -> UTCTime -> Maybe UTCTime -> Maintenance_
fromMaintenanceQL (MaintenanceArg {..}) cd md = Maintenance_ { maintenance_Name = name
                                                             , maintenance_Description = description
                                                             , maintenance_Status = readEntityStatus status
                                                             , maintenance_CreatedDate = cd
                                                             , maintenance_ModifiedDate = md
                                                             }
