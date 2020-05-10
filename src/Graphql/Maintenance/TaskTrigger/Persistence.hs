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

module Graphql.Maintenance.TaskTrigger.Persistence ( saveTaskTriggers, taskTriggerQuery ) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils hiding(unionFilters, conjunctionFilters, getOperator)
import Data.Time
import Graphql.Maintenance.Task.DataTypes
import Graphql.Maintenance.TaskTrigger.DataTypes
import Graphql.Maintenance.SubTask.Persistence

--taskTriggerQuery :: Task_Id -> Handler [Entity TaskTrigger_]
--taskTriggerQuery taskId =  do
--                      result <- runDB
--                                   $ E.select
--                                   $ E.from $ \task -> do
--                                        let subquery =
--                                              E.from $ \maintenanceTask -> do
--                                              E.where_ (maintenanceTask ^. MaintenanceTask_MaintenanceId E.==. E.val taskId)
--                                              return (maintenanceTask ^. MaintenanceTask_TaskId)
--                                        E.where_ (task ^. Task_Id `E.in_` E.subList_select subquery)
--                                        return task
--                      return result

taskTriggerQuery :: Task_Id -> Handler [Entity TaskTrigger_]
taskTriggerQuery taskId =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \taskTrigger -> do
                                        E.where_ (taskTrigger ^. TaskTrigger_TaskId E.==. E.val taskId)
                                        return taskTrigger
                      return result


--saveTaskTriggers :: Task_Id -> [TaskTriggerArg] -> Handler [Task_Id]
saveTaskTriggers _ [] = pure []
saveTaskTriggers taskId (x:xs) = do
                                  taskTriggerId <-  createOrUpdateTaskTrigger taskId x
                                  taskTriggerIds <- saveTaskTriggers taskId xs
                                  return (taskTriggerId:taskTriggerIds)

--createOrUpdateTaskTrigger :: TaskTriggerArg -> Handler (TaskTrigger MutRes)
createOrUpdateTaskTrigger taskId taskTrigger = do
                let TaskTriggerArg {..} = taskTrigger
                now <- liftIO getCurrentTime
                entityId <- if taskTriggerId > 0 then
                                do
                                  let taskTriggerKey = (toSqlKey $ fromIntegral $ taskTriggerId)::TaskTrigger_Id
                                  _ <- runDB $ update taskTriggerKey [ TaskTrigger_Kind =. kind
                                                                     , TaskTrigger_Description =. description
                                                                     , TaskTrigger_FixedSchedule =. fixedSchedule
                                                                     , TaskTrigger_Frequency =. frequency
                                                                     , TaskTrigger_ReadType =. readType
                                                                     , TaskTrigger_Limit =. limit
                                                                     , TaskTrigger_Repeat =. repeat
                                                                     , TaskTrigger_Operator =. operator
                                                                     , TaskTrigger_Value =. value
                                                                     , TaskTrigger_TimeFrequency =. (case timeFrequency of Nothing -> Nothing; Just t -> Just $ readTimeFrequency t)
                                                                     , TaskTrigger_UnitId =. case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                                                     , TaskTrigger_EventTriggerId =. case eventTriggerId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::EventTrigger_Id)
                                                                     , TaskTrigger_TaskId =. taskId
                                                                     , TaskTrigger_ModifiedDate =. Just now
                                                                     ]
                                  return taskTriggerKey
                               else do
                                  taskTriggerKey <- runDB $ insert $ fromTaskTriggerQL taskId taskTrigger now Nothing
                                  return taskTriggerKey
                return entityId

fromTaskTriggerQL :: Task_Id -> TaskTriggerArg -> UTCTime -> Maybe UTCTime -> TaskTrigger_
fromTaskTriggerQL taskId (TaskTriggerArg {..}) cd md = TaskTrigger_ { taskTrigger_Kind = kind
                                                                    , taskTrigger_Description = description
                                                                    , taskTrigger_FixedSchedule = fixedSchedule
                                                                    , taskTrigger_Frequency = frequency
                                                                    , taskTrigger_ReadType = readType
                                                                    , taskTrigger_Limit = limit
                                                                    , taskTrigger_Repeat = repeat
                                                                    , taskTrigger_Operator = operator
                                                                    , taskTrigger_Value = value
                                                                    , taskTrigger_TimeFrequency = (case timeFrequency of Nothing -> Nothing; Just t -> Just $ readTimeFrequency t)
                                                                    , taskTrigger_UnitId = case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                                                    , taskTrigger_EventTriggerId = case eventTriggerId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::EventTrigger_Id)
                                                                    , taskTrigger_TaskId = taskId
                                                                    , taskTrigger_CreatedDate = cd
                                                                    , taskTrigger_ModifiedDate = md
                                                                    }
