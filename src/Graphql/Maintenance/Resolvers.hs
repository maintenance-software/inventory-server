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

module Graphql.Maintenance.Resolvers (
      maintenanceResolver
    , getMaintenanceByIdResolver_
    , saveMaintenanceResolver
    , toMaintenanceQL
) where

import Import
import Data.Morpheus.Types (lift)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums ()
import Graphql.Utils
import Graphql.Maintenance.Task.Resolvers
import Graphql.Maintenance.Task.Persistence
import Graphql.Asset.Equipment.Resolvers
import Graphql.Maintenance.DataTypes
import Graphql.Maintenance.Persistence
import Graphql.Maintenance.TaskTrigger.EventTrigger
import Graphql.Maintenance.Task.DataTypes (Task(..))
import Graphql.Asset.Equipment.DataTypes (Equipment(..))
--maintenanceResolver :: () -> Res e Handler Maintenances
maintenanceResolver _ = pure Maintenances { maintenance = getMaintenanceByIdResolver
                                          , page = maintenancePageResolver
                                          , saveMaintenance = saveMaintenanceResolver
                                          , task = getTaskByIdResolver
                                          , createUpdateTasks = createUpdateTasksResolver
                                          , eventTriggers = listEventTriggerResolver
                                          , saveEventTrigger = saveEventTriggerResolver
                                          }

--getMaintenanceByIdResolver :: GetEntityByIdArg -> Res e Handler (Maintenance Res)
getMaintenanceByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => GetEntityByIdArg -> o () Handler (Maintenance o)
getMaintenanceByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let maintenanceId = (toSqlKey $ fromIntegral $ entityId)::Maintenance_Id
                                              maintenance <- runDB $ getJustEntity maintenanceId
                                              return $ toMaintenanceQL maintenance

getMaintenanceByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Maintenance_Id -> () -> o () Handler (Maintenance o)
getMaintenanceByIdResolver_ maintenanceId _ = lift $ do
                                    maintenance <- runDB $ getJustEntity maintenanceId
                                    return $ toMaintenanceQL maintenance

maintenancePageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (Maintenance o))
maintenancePageResolver page = lift $ do
                        countItems <- maintenanceQueryCount page
                        queryResult <- maintenanceQuery page
                        let result = P.map (\ m -> toMaintenanceQL m) queryResult
                        return Page { totalCount = countItems
                                    , content = result
                                    , pageInfo = PageInfo { hasNext = (pageIndex_ * pageSize_ + pageSize_ < countItems)
                                                          , hasPreview = pageIndex_ * pageSize_ > 0
                                                          , pageSize = pageSize_
                                                          , pageIndex = pageIndex_
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex_ = case pageIndex of Just  x  -> x; Nothing -> 0
                            pageSize_ = case pageSize of Just y -> y; Nothing -> 10

equipmentResolver_ :: (MonadTrans t, MonadTrans (o ())) => Maintenance_Id -> p -> t Handler [Equipment o]
equipmentResolver_ maintenanceId _ = lift $ do
                              itemEquipments <- equipmentQuery maintenanceId
                              let result = P.map (\(e, i) -> toEquipmentQL e i) itemEquipments
                              return result

--saveMaintenanceResolver :: MaintenanceArg -> MutRes e Handler (Maintenance MutRes)
saveMaintenanceResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => MaintenanceArg -> t Handler (Maintenance o)
saveMaintenanceResolver arg = lift $ do
                                  maintenanceId <- createOrUpdateMaintenance arg
                                  maintenance <- runDB $ getJustEntity maintenanceId
                                  return $ toMaintenanceQL maintenance

createUpdateTasksResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => MaintenanceTaskArg -> t Handler [Task o]
createUpdateTasksResolver MaintenanceTaskArg {..} = lift $ do
                         let entityId = (toSqlKey $ fromIntegral $ maintenanceId)::Maintenance_Id
                         _ <- saveTasks entityId tasks
                         entityTasks <- taskQuery entityId
                         return $ P.map (\t -> toTaskQL t) entityTasks

-- CONVERTERS
--toMaintenanceQL :: Entity Maintenance_ -> Maintenance
toMaintenanceQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Maintenance_ -> Maintenance o
toMaintenanceQL (Entity maintenanceId maintenance) = Maintenance { maintenanceId = fromIntegral $ fromSqlKey maintenanceId
                                                                 , name = maintenance_Name
                                                                 , description = maintenance_Description
                                                                 , status = T.pack $ show maintenance_Status
                                                                 , tasks = taskResolver_ maintenanceId
                                                                 , equipments = equipmentResolver_ maintenanceId
                                                                 , createdDate = fromString $ show maintenance_CreatedDate
                                                                 , modifiedDate = m
                                                                 }
                                          where
                                            Maintenance_ {..} = maintenance
                                            m = case maintenance_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

{-
query {
  inventories(queryString: "") {
    maintenanceId
    name
    description
  }
}

mutation {
  saveCategory(maintenanceId: 0, name: "test", description: "sss") {
    maintenanceId
    name
  }
}
-}
