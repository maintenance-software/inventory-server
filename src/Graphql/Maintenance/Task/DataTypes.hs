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

module Graphql.Maintenance.Task.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Enums
import Data.Time
import Graphql.Maintenance.Task.TaskCategory
import Graphql.Maintenance.SubTask.DataTypes

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
                   , taskCategory :: Maybe(() -> o () Handler TaskCategory)
                   , subTasks :: () -> o () Handler [SubTask o]
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
                       , subTasks :: [SubTaskArg]
                       } deriving (Generic)

instance GQLType TaskArg where
    type  KIND TaskArg = INPUT_OBJECT
    description = const $ Just $ pack "This field holds Task Input information"

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
