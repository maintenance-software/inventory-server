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

module Graphql.Maintenance.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Types (GQLType)
import Graphql.Utils (GetEntityByIdArg, Page, PageArg)
import Graphql.Maintenance.Task.DataTypes
import Graphql.Asset.Equipment.DataTypes
import Graphql.Maintenance.TaskTrigger.EventTrigger

data Maintenance o = Maintenance { maintenanceId :: Int
                                 , name :: Text
                                 , description :: Maybe Text
                                 , status :: Text
                                 , createdDate :: Text
                                 , modifiedDate :: Maybe Text
                                 , tasks :: () -> o () Handler [Task o]
                                 , equipments :: () -> o () Handler [Equipment o]
                                 } deriving (Generic, GQLType)

data TaskActivity = TaskActivity { taskActivityId :: Int
                                 , scheduledDate :: Maybe Text
                                 , calculatedDate :: Text
                                 , rescheduled :: Bool
                                 , status :: Text
                                 , assetId :: Int
                                 , assetName :: Text
                                 , maintenanceId :: Int
                                 , maintenanceName :: Text
                                 , taskId :: Int
                                 , taskName :: Text
                                 , taskPriority :: Int
                                 } deriving (Generic, GQLType)

data Maintenances o = Maintenances { maintenance :: GetEntityByIdArg ->  o () Handler (Maintenance o)
                                   , page :: PageArg -> o () Handler (Page (Maintenance o))
                                   , availableEquipments :: PageArg -> o () Handler (Page (Equipment o))
                                   , taskActivities :: PageArg -> o () Handler (Page TaskActivity)
                                   , saveMaintenance :: MaintenanceArg -> o () Handler (Maintenance o)
                                   , createUpdateTasks :: MaintenanceTaskArg -> o () Handler [Task o]
                                   , task :: GetEntityByIdArg -> o () Handler (Task o)
                                   , eventTriggers :: () -> o () Handler [EventTrigger]
                                   , saveEventTrigger :: EventTriggerArg -> o () Handler EventTrigger
                                   } deriving (Generic, GQLType)

data MaintenanceArg = MaintenanceArg { maintenanceId :: Int
                                     , name :: Text
                                     , description :: Maybe Text
                                     , status :: Text
                                     } deriving (Generic)

data MaintenanceTaskArg = MaintenanceTaskArg { maintenanceId :: Int
                                             , tasks :: [TaskArg]
                                             } deriving (Generic)
