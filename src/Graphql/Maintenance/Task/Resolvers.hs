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

module Graphql.Maintenance.Task.Resolvers (
      taskResolver_
    , getTaskByIdResolver
    , toTaskQL
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
import Graphql.Maintenance.Task.TaskCategory
import Graphql.Maintenance.SubTask.Resolvers
import Graphql.Maintenance.TaskTrigger.Resolvers
import Graphql.Maintenance.Task.DataTypes
import Graphql.Maintenance.Task.Persistence

taskResolver_ maintenanceId _ = lift $ do
                                tasks <- taskQuery maintenanceId
                                return $ P.map (\t -> toTaskQL t) tasks

--getTaskByIdResolver :: GetEntityByIdArg -> Res e Handler (Task Res)
getTaskByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let taskId = (toSqlKey $ fromIntegral $ entityId)::Task_Id
                                              task <- runDB $ getJustEntity taskId
                                              return $ toTaskQL task

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
                                     , taskCategory = case task_TaskCategoryId of Nothing -> Nothing; Just c -> Just $ getTaskCategoryByIdResolver_ c
                                     , subTasks = subTaskResolver_ taskId
                                     , taskTriggers = taskTriggerResolver_ taskId
                                     , createdDate = fromString $ show task_CreatedDate
                                     , modifiedDate = m
                                     }
                                          where
                                            Task_ {..} = task
                                            m = case task_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

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
