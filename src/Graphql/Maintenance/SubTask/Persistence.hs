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

module Graphql.Maintenance.SubTask.Persistence (
      subTaskQuery
    , saveSubTasks
    , createOrUpdateSubTask
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils hiding(unionFilters, conjunctionFilters, getOperator)
import Data.Time
import Graphql.Maintenance.SubTask.SubTaskKind
import Graphql.Maintenance.SubTask.DataTypes

subTaskQuery :: Task_Id -> Handler [Entity SubTask_]
subTaskQuery taskId =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \subTask -> do
                                        E.where_ (subTask ^. SubTask_TaskId E.==. E.val taskId)
                                        return subTask
                      return result

--saveSubTasks :: SubTask_Id -> [SubTaskArg] -> Handler [SubTask_Id]
saveSubTasks _ [] = pure []
saveSubTasks taskId (x:xs) = do
                                  subTaskId <-  createOrUpdateSubTask taskId x
                                  taskIds <- saveSubTasks taskId xs
                                  return (subTaskId:taskIds)

--createOrUpdateSubTask :: MaintenanceArg -> Handler (SubTask MutRes)
createOrUpdateSubTask taskId subTask = do
                let SubTaskArg {..} = subTask
                now <- liftIO getCurrentTime
                entityId <- if subTaskId > 0 then
                                do
                                  let subTaskKey = (toSqlKey $ fromIntegral $ subTaskId)::SubTask_Id
                                  _ <- runDB $ update subTaskKey [ SubTask_Order =. order
                                                                 , SubTask_Group =. group
                                                                 , SubTask_Description =. description
                                                                 , SubTask_Mandatory =. mandatory
                                                                 , SubTask_SubTaskKindId  =. case subTaskKindId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::SubTaskKind_Id)
                                                                 , SubTask_TaskId =. taskId
                                                                 , SubTask_ModifiedDate =. Just now
                                                                 ]
                                  return subTaskKey
                               else do
                                  subTaskKey <- runDB $ insert $ fromTaskQL taskId subTask now Nothing
                                  return subTaskKey
                return entityId

fromTaskQL :: Task_Id -> SubTaskArg -> UTCTime -> Maybe UTCTime -> SubTask_
fromTaskQL taskId (SubTaskArg {..}) cd md = SubTask_ { subTask_Order = order
                                                     , subTask_Group = group
                                                     , subTask_Description = description
                                                     , subTask_Mandatory = mandatory
                                                     , subTask_SubTaskKindId  = case subTaskKindId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::SubTaskKind_Id)
                                                     , subTask_TaskId = taskId
                                                     , subTask_CreatedDate = cd
                                                     , subTask_ModifiedDate = md
                                                     }
