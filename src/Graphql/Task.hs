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

module Graphql.Task (
      taskResolver_
    , getTaskByIdResolver
    , saveTasks
    , toTaskQL
    , Task
    , TaskArg
) where

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
import Graphql.TaskCategory

data Task o = Task { taskId :: Int
                   , name :: Text
                   , description :: Maybe Text
                   , priority :: Int
                   , duration :: Int
                   , downTimeDuration :: Int
                   , attribute1 :: Maybe Text
                   , attribute2 :: Maybe Text
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   , taskCategoryArg :: Maybe(() -> o () Handler TaskCategory)
                   } deriving (Generic, GQLType)

data TaskArg = TaskArg { taskId :: Int
                       , name :: Text
                       , description :: Maybe Text
                       , priority :: Int
                       , duration :: Int
                       , downTimeDuration :: Int
                       , attribute1 :: Maybe Text
                       , attribute2 :: Maybe Text
                       , taskCategoryId :: Maybe Int
                       } deriving (Generic)

instance GQLType TaskArg where
    type  KIND TaskArg = INPUT_OBJECT
    description = const $ Just $ pack "This field holds Task Input information"

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

taskResolver_ maintenanceId _ = lift $ do
                                tasks <- taskQuery maintenanceId
                                return $ P.map (\t -> toTaskQL t) tasks

--getTaskByIdResolver :: GetEntityByIdArg -> Res e Handler (Task Res)
getTaskByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let taskId = (toSqlKey $ fromIntegral $ entityId)::Task_Id
                                              task <- runDB $ getJustEntity taskId
                                              return $ toTaskQL task



--saveTaskResolver_ maintenanceId arg = lift $ do
--                                  taskId <- createOrUpdateTask maintenanceId arg
--                                  task <- runDB $ getJustEntity taskId
--                                  return $ toTaskQL task

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
                return entityId

-- CONVERTERS
--toTaskQL :: Entity Task_ -> Task
toTaskQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Task_ -> Task o
toTaskQL (Entity taskId task) = Task { taskId = fromIntegral $ fromSqlKey taskId
                                     , name = task_Name
                                     , description = task_Description
                                     , priority = task_Priority
                                     , duration = task_Duration
                                     , downTimeDuration = task_DownTimeDuration
                                     , attribute1 = task_Attribute1
                                     , attribute2 = task_Attribute2
                                     , taskCategoryArg = case task_TaskCategoryId of Nothing -> Nothing; Just c -> Just $ getTaskCategoryByIdResolver_ c
                                     , createdDate = fromString $ show task_CreatedDate
                                     , modifiedDate = m
                                     }
                                          where
                                            Task_ {..} = task
                                            m = case task_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

fromTaskQL :: Maintenance_Id -> TaskArg -> UTCTime -> Maybe UTCTime -> Task_
fromTaskQL maintenanceId (TaskArg {..}) cd md = Task_ { task_Name = name
                                        , task_Description = description
                                        , task_Priority = priority
                                        , task_Duration = duration
                                        , task_DownTimeDuration = downTimeDuration
                                        , task_Attribute1 = attribute1
                                        , task_Attribute2 = attribute2
                                        , task_TaskCategoryId = case taskCategoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::TaskCategory_Id)
                                        , task_MaintenanceId = maintenanceId
                                        , task_CreatedDate = cd
                                        , task_ModifiedDate = md
                                        }

{-
query {
  inventories(queryString: "") {
    taskId
    name
    description
  }
}

mutation {
  saveCategory(taskId: 0, name: "test", description: "sss") {
    taskId
    name
  }
}
-}
