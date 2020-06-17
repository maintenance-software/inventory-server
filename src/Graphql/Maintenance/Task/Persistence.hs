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

module Graphql.Maintenance.Task.Persistence ( saveTasks, taskQuery, getTaskByIds ) where

import Import
import Database.Persist.Sql (toSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), in_)
import Graphql.Maintenance.Task.DataTypes
import Graphql.Maintenance.SubTask.Persistence
import Graphql.Maintenance.TaskTrigger.Persistence
import Graphql.Maintenance.Task.TaskResource (saveTaskResources)

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
                                        E.where_ (task ^. Task_MaintenanceId E.==. E.val (Just maintenanceId))
                                        return task
                      return result

getTaskByIds :: [Task_Id] -> Handler [Entity Task_]
getTaskByIds taskIds =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \task -> do
                                        E.where_ (task ^. Task_Id `in_` E.valList taskIds)
                                        return task
                      return result

saveTasks :: Maybe Maintenance_Id -> [TaskArg] -> HandlerFor App [Task_Id]
saveTasks _ [] = pure []
saveTasks maintenanceId (x:xs) = do
                                  taskId <-  createOrUpdateTask maintenanceId x
                                  taskIds <- saveTasks maintenanceId xs
                                  return (taskId:taskIds)

createOrUpdateTask :: Maybe Maintenance_Id -> TaskArg -> Handler Task_Id
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
                                                              , Task_TaskCategoryId =. case taskCategoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Category_Id)
                                                              , Task_MaintenanceId =. maintenanceId
                                                              , Task_ModifiedDate =. Just now
                                                              ]
                                  return taskKey
                               else do
                                  taskKey <- runDB $ insert $ fromTaskQL maintenanceId task now Nothing
                                  return taskKey
                _ <- saveSubTasks entityId subTasks
                _ <- saveTaskTriggers entityId taskTriggers
                _ <- saveTaskResources entityId taskResources
                return entityId

fromTaskQL :: Maybe Maintenance_Id -> TaskArg -> UTCTime -> Maybe UTCTime -> Task_
fromTaskQL maintenanceId (TaskArg {..}) cd md = Task_ { task_Name = name
                                        , task_Description = description
                                        , task_Priority = priority
                                        , task_Duration = duration
                                        , task_DownTimeDuration = downTimeDuration
                                        , task_Attribute1 = attribute1
                                        , task_Attribute2 = attribute2
                                        , task_TaskCategoryId = case taskCategoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Category_Id)
                                        , task_MaintenanceId = maintenanceId
                                        , task_CreatedDate = cd
                                        , task_ModifiedDate = md
                                        }
