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
import GHC.Generics ()
import Data.Morpheus.Types (GQLType(..))
import Graphql.Utils (EntityIdArg, Page, PageArg)
import Graphql.Maintenance.Task.DataTypes
import Graphql.Asset.Equipment.DataTypes ()
import Graphql.Asset.DataTypes ()
import Graphql.Category ()
import Graphql.Utils ()
import Graphql.DataTypes (Equipment)

data Maintenance o = Maintenance { maintenanceId :: Int
                                 , name :: Text
                                 , description :: Maybe Text
                                 , status :: Text
                                 , createdDate :: Text
                                 , modifiedDate :: Maybe Text
                                 , tasks :: () -> o () Handler [Task o]
                                 , equipments :: () -> o () Handler [Equipment o]
                                 } deriving (Generic, GQLType)

data Maintenances o = Maintenances { maintenance :: EntityIdArg ->  o () Handler (Maintenance o)
                                   , page :: PageArg -> o () Handler (Page (Maintenance o))
                                   , availableEquipments :: PageArg -> o () Handler (Page (Equipment o))
                                   , addWorkQueueDate :: WorkQueueDateArg -> o () Handler Bool
                                   , addWorkQueueEvent :: WorkQueueEventArg -> o () Handler Bool
                                   , saveMaintenance :: MaintenanceArg -> o () Handler (Maintenance o)
                                   , createUpdateTasks :: MaintenanceTaskArg -> o () Handler [Task o]
                                   , task :: EntityIdArg -> o () Handler (Task o)
                                   , equipmentTasks :: EntityIdArg -> o () Handler [Task o]
                                   } deriving (Generic, GQLType)

data MaintenanceArg = MaintenanceArg { maintenanceId :: Int
                                     , name :: Text
                                     , description :: Maybe Text
                                     , status :: Text
                                     } deriving (Generic)

data MaintenanceTaskArg = MaintenanceTaskArg { maintenanceId :: Maybe Int
                                             , tasks :: [TaskArg]
                                             } deriving (Generic)

data WorkQueueDateArg = WorkQueueDateArg { lastMaintenanceDate :: Text
                                         , assetId :: Int
                                         , maintenanceId :: Int
                                         } deriving (Generic)

data WorkQueueEventArg = WorkQueueEventArg { assetId :: Int
                                           , taskId :: Int
                                           , taskTriggerId :: Int
                                           , maintenanceId :: Maybe Int
                                           , reportedById :: Int
                                           , hasAssetFailure :: Bool
                                           , incidentDate :: Maybe Text
                                           } deriving (Generic)
