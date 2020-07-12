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
import qualified Data.Text as T
import Enums ()
import Graphql.Utils
--import Graphql.Maintenance.Resolvers (getWorkQueueByIdResolver_)
import Graphql.WorkOrder.Persistence
--import Graphql.Maintenance.Persistence (fetchWorkQueuesByWorkOrderIdQuery)
--import Graphql.DataTypes (Equipment(..))
import Graphql.Admin.Person (getPersonByIdResolver_)
--import Graphql.Asset.Equipment.Resolvers (toEquipmentQL)
--import Graphql.Asset.InventoryItem.Resolvers (getInventoryItemByIdResolver_)
--import Graphql.Maintenance.SubTask.Resolvers (getSubTaskByIdResolver_)
import Graphql.DataTypes (WorkQueue, WorkOrders(..), WorkOrder(..), WorkOrderProgressArg(..), WorkOrderArg, WorkOrderResourceArg)
import Graphql.WorkQueue.Resolvers (toWorkQueueQL)

workOrderResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (WorkOrders o)
workOrderResolver _ = pure WorkOrders {  workOrder = getWorkOrderByIdResolver
                                       , createUpdateWorkOrder = createUpdateWorkOrderResolver
                                       , page = workOrderPageResolver
                                       , changeStatus = workOrderChangeStatusResolver
                                       , saveWorkOrderProgress = saveWorkOrderProgressResolver
                                       , saveWorkOrderResources = saveWorkOrderResourcesResolver
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

fetchWorkQueuesByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [WorkQueue o]
fetchWorkQueuesByWorkOrderIdResolver_ workOrderId _ = lift $ do
                              workQueues <- runDB $ selectList [WorkQueue_WorkOrderId ==. Just workOrderId] []
                              let result = P.map (\ wq -> toWorkQueueQL wq) workQueues
                              return result

--fetchWorkResourcesByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [WorkOrderResource o]
--fetchWorkResourcesByWorkOrderIdResolver_ workOrderId _ = lift $ do
--                              workOrderResources <-  runDB $ selectList [WorkOrderResource_WorkOrderId ==. workOrderId] []
--                              return $ P.map (\ r -> toWorkOrderResourceQL r) workOrderResources
--
--fetchWorkOrderSubtaskByWorkOrderIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrder_Id -> () -> o () Handler [WorkOrderSubTask o]
--fetchWorkOrderSubtaskByWorkOrderIdResolver_ workOrderId _ = lift $ do
--                              workOrderSubtask <-  runDB $ selectList [WorkOrderSubTask_WorkOrderId ==. workOrderId] []
--                              return $ P.map (\ r -> toWorkOrderSubTaskQL r) workOrderSubtask

createUpdateWorkOrderResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => WorkOrderArg -> o () Handler (WorkOrder o)
createUpdateWorkOrderResolver arg = lift $ do
                         workOrderId <- createUpdateWorkOrderPersistent arg
                         workOrder <-  runDB $ getJustEntity workOrderId
                         return $ toWorkOrderQL workOrder

saveWorkOrderResourcesResolver :: (MonadTrans t) => EntityArg [WorkOrderResourceArg] -> t Handler Bool
saveWorkOrderResourcesResolver EntityArg{..} = lift $ do
                                              _ <- saveWorkOrderResource arg
                                              return True

saveWorkOrderProgressResolver :: (MonadTrans t) => WorkOrderProgressArg -> t Handler Bool
saveWorkOrderProgressResolver WorkOrderProgressArg{..} = lift $ do
                                              now <- liftIO getCurrentTime
                                              let workOrderEntityId = ((toSqlKey $ fromIntegral $ workOrderId)::WorkOrder_Id)
                                              let workQueueEntityId = ((toSqlKey $ fromIntegral $ workQueueId)::WorkQueue_Id)
                                              _ <- runDB $ update workQueueEntityId [ WorkQueue_StartWorkDate =. Just ((read $ T.unpack startWorkDate)::UTCTime)
                                                                                    , WorkQueue_FinishedWorkDate =. Just ((read $ T.unpack finishedWorkDate)::UTCTime)
                                                                                    , WorkQueue_Notes =. (Just notes)
                                                                                    , WorkQueue_Status =. status
                                                                                    , WorkQueue_ModifiedDate =. Just now
                                                                                    ]
                                              _ <- mapM (createUpdateWorkOrderSubTask workQueueEntityId)  workOrderSubTasks
                                              workOrderQueues <-  runDB $ selectList [WorkQueue_WorkOrderId ==. Just workOrderEntityId] []
                                              let tasksCompleted = P.filter (\(Entity _ WorkQueue_{..}) -> workQueue_Status == "WO_TASK_COMPLETED") workOrderQueues
                                              let p = 100 * (fromIntegral $ P.length tasksCompleted) / (fromIntegral $ P.length  workOrderQueues)
                                              _ <- runDB $ update workOrderEntityId [ WorkOrder_Percentage =. p, WorkOrder_ModifiedDate =. Just now]
                                              return True

workOrderChangeStatusResolver :: (MonadTrans t) => EntityChangeStatusArg -> t Handler Bool
workOrderChangeStatusResolver workOrderRequestStatus = lift $ do
                                                      sucess <-  changeWorkOrderStatus workOrderRequestStatus
                                                      return sucess

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
                                                         , generatedBy = (case workOrder_GeneratedById of Nothing -> Nothing; Just a -> Just $ getPersonByIdResolver_ a)
                                                         , responsible = (case workOrder_ResponsibleId of Nothing -> Nothing; Just a -> Just $ getPersonByIdResolver_ a)
                                                         , parent = (case workOrder_ParentId of Nothing -> Nothing; Just a -> Just $ getWorkOrderByIdResolver_ a)
                                                         , workQueues = fetchWorkQueuesByWorkOrderIdResolver_ workOrderId
--                                                         , workOrderResources = fetchWorkResourcesByWorkOrderIdResolver_ workOrderId
--                                                         , workOrderSubTask = fetchWorkOrderSubtaskByWorkOrderIdResolver_ workOrderId
                                                         , createdDate = fromString $ show workOrder_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            WorkOrder_ {..} = workOrder
                                            m = case workOrder_ModifiedDate of
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
