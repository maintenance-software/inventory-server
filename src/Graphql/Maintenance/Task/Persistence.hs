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

module Graphql.Maintenance.Task.Persistence ( saveTasks, taskQuery ) where

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
import Graphql.Maintenance.SubTask.Persistence
import Graphql.Maintenance.TaskTrigger.Persistence

--taskQuery :: Maintenance_Id -> Handler [Entity Task_]
--taskQuery maintenanceId =  do
--                      result <- runDB
--                                   $ E.select
--                                   $ E.from $ \task -> do
--                                        let subquery =
--                                              E.from $ \maintenanceTask -> do
--                                              E.where_ (maintenanceTask ^. MaintenanceTask_MaintenanceId E.==. E.val maintenanceId)
--                                              return (maintenanceTask ^. MaintenanceTask_TaskId)
--                                        E.where_ (task ^. Task_Id `E.in_` E.subList_select subquery)
--                                        return task
--                      return result

taskQuery :: Maintenance_Id -> Handler [Entity Task_]
taskQuery maintenanceId =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \task -> do
                                        E.where_ (task ^. Task_MaintenanceId E.==. E.val maintenanceId)
                                        return task
                      return result


--saveTasks :: Maintenance_Id -> [TaskArg] -> Handler [Task_Id]
saveTasks _ [] = pure []
saveTasks maintenanceId (x:xs) = do
                                  taskId <-  createOrUpdateTask maintenanceId x
                                  taskIds <- saveTasks maintenanceId xs
                                  return (taskId:taskIds)

--createOrUpdateTask :: MaintenanceArg -> Handler (Task MutRes)
createOrUpdateTask maintenanceId task = do
                let TaskArg {..} = task
                now <- liftIO getCurrentTime
                entityId <- if taskId > 0 then
                                do
                                  let taskKey = (toSqlKey $ fromIntegral $ taskId)::Task_Id
                                  _ <- runDB $ update taskKey [ Task_Name =. name
                                                              , Task_Description =. description
                                                              , Task_Priority =. priority
                                                              , Task_Duration =. duration
                                                              , Task_DownTimeDuration =. downTimeDuration
                                                              , Task_Attribute1 =. attribute1
                                                              , Task_Attribute2 =. attribute2
                                                              , Task_TaskCategoryId =. case taskCategoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::TaskCategory_Id)
                                                              , Task_MaintenanceId =. maintenanceId
                                                              , Task_ModifiedDate =. Just now
                                                              ]
                                  return taskKey
                               else do
                                  taskKey <- runDB $ insert $ fromTaskQL maintenanceId task now Nothing
                                  return taskKey
                subTaskIds <- saveSubTasks entityId subTasks
                taskTriggerIds <- saveTaskTriggers entityId taskTriggers
                return entityId
