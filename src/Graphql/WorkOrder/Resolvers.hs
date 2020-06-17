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

module Graphql.WorkOrder.Resolvers (
      workOrderResolver
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Enums ()
import Graphql.Utils
import Graphql.WorkOrder.DataTypes
import Graphql.Maintenance.Resolvers (getWorkQueueByIdResolver_)
import Graphql.WorkOrder.Persistence
import Graphql.Maintenance.Persistence (fetchWorkQueuesByWorkOrderIdQuery)
import Graphql.DataTypes (Equipment(..))
import Graphql.Admin.Person (getPersonByIdResolver_)
import Graphql.Asset.Equipment.Resolvers (toEquipmentQL)
import Graphql.Asset.InventoryItem.Resolvers (getInventoryItemByIdResolver_)

workOrderResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (WorkOrders o)
workOrderResolver _ = pure WorkOrders { workOrder = getWorkOrderByIdResolver
                                       , createUpdateWorkOrder = createUpdateWorkOrderResolver
                                       , page = workOrderPageResolver
                                       }

getWorkOrderByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (WorkOrder o)
getWorkOrderByIdResolver EntityIdArg {..} = lift $ do
                                              let workOrderId = (toSqlKey $ fromIntegral $ entityId)::WorkOrder_Id
                                              workOrder <- runDB $ getJustEntity workOrderId
                                              return $ toWorkOrderQL workOrder

getWorkOrderByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler (WorkOrder o)
getWorkOrderByIdResolver_ workOrderId _ = lift $ do
                                              workOrder <- runDB $ getJustEntity workOrderId
                                              return $ toWorkOrderQL workOrder

workOrderPageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (WorkOrder o))
workOrderPageResolver page = lift $ do
                        countItems <- workOrderQueryCount page
                        queryResult <- workOrderQuery page
                        let result = P.map (\ wo -> toWorkOrderQL wo) queryResult
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
                            pageIndex_ = case pageIndex of Just x -> x; Nothing -> 0
                            pageSize_ = case pageSize of Just y -> y; Nothing -> 10

fetchEquipmentsByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [Equipment o]
fetchEquipmentsByWorkOrderIdResolver_ workOrderId _ = lift $ do
                              workQueues <- fetchWorkQueuesByWorkOrderIdQuery workOrderId
                              let result = P.map (\ (e, i) -> toEquipmentQL e i) workQueues
                              return result

fetchWorkResourcesByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [WorkOrderResource o]
fetchWorkResourcesByWorkOrderIdResolver_ workOrderId _ = lift $ do
                              workOrderResources <-  runDB $ selectList [WorkOrderResource_WorkOrderId ==. workOrderId] []
                              return $ P.map (\ r -> toWorkOrderResourceQL r) workOrderResources

createUpdateWorkOrderResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrderArg -> o () Handler (WorkOrder o)
createUpdateWorkOrderResolver arg = lift $ do
                         workOrderId <- createUpdateWorkOrderPersistent arg
                         workOrder <-  runDB $ getJustEntity workOrderId
                         return $ toWorkOrderQL workOrder

-- CONVERTERS
toWorkOrderQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrder_ -> WorkOrder o
toWorkOrderQL (Entity workOrderId workOrder) = WorkOrder { workOrderId = fromIntegral $ fromSqlKey workOrderId
                                                         , workOrderCode = workOrder_WorkOrderCode
                                                         , workOrderStatus = workOrder_WorkOrderStatus
                                                         , estimateDuration = workOrder_EstimateDuration
                                                         , executionDuration = workOrder_ExecutionDuration
                                                         , rate = workOrder_Rate
                                                         , totalCost = realToFrac workOrder_TotalCost
                                                         , percentage = realToFrac workOrder_Percentage
                                                         , notes = workOrder_Notes
                                                         , generatedBy = getPersonByIdResolver_ workOrder_GeneratedById
                                                         , responsible = getPersonByIdResolver_ workOrder_ResponsibleId
                                                         , parent = (case workOrder_ParentId of Nothing -> Nothing; Just a -> Just $ getWorkOrderByIdResolver_ a)
                                                         , equipments = fetchEquipmentsByWorkOrderIdResolver_ workOrderId
                                                         , workOrderResources = fetchWorkResourcesByWorkOrderIdResolver_ workOrderId
                                                         , createdDate = fromString $ show workOrder_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            WorkOrder_ {..} = workOrder
                                            m = case workOrder_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing


toWorkOrderResourceQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity WorkOrderResource_ -> WorkOrderResource o
toWorkOrderResourceQL (Entity workOrderResourceId workOrderResource) = WorkOrderResource { workOrderResourceId = fromIntegral $ fromSqlKey workOrderResourceId
                                                                                         , amount = workOrderResource_Amount
                                                                                         , humanResource =  (case workOrderResource_HumanResourceId of Nothing -> Nothing; Just a -> Just $ getPersonByIdResolver_ a)
                                                                                         , inventoryItem =  (case workOrderResource_InventoryItemId of Nothing -> Nothing; Just a -> Just $ getInventoryItemByIdResolver_ a)
                                                                                         , workQueue = getWorkQueueByIdResolver_ workOrderResource_WorkQueueId
                                                                                         , createdDate = fromString $ show workOrderResource_CreatedDate
                                                                                         , modifiedDate = m
                                                                                         }
                                                                        where
                                                                          WorkOrderResource_ {..} = workOrderResource
                                                                          m = case workOrderResource_ModifiedDate of
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
