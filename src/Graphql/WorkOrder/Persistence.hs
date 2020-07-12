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

module Graphql.WorkOrder.Persistence (
        workOrderQueryCount
      , workOrderQuery
      , createUpdateWorkOrderPersistent
      , changeWorkOrderStatus
      , createUpdateWorkOrderSubTask
) where

import Import hiding (union)
--import Data.Time.Calendar (toGregorian, fromGregorian)
--import Data.Text.Time (parseISODateTime)
--import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), notIn, in_, {-countRows-})
--import Database.Esqueleto.PostgreSQL (arrayAggDistinct)
import Prelude as P
import qualified Data.Text as T
import Graphql.Utils
import Data.Time ()
import Graphql.DataTypes (WorkOrderResourceArg(..), WorkOrderArg(..), WorkOrderSubTaskArg(..))

workOrderPredicate :: E.SqlExpr (Entity WorkOrder_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
workOrderPredicate workOrder Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                            | T.strip field == "workOrderCode" = [getOperator operator (workOrder ^. WorkOrder_WorkOrderCode) (E.val $ T.strip value)]
                                            | T.strip field == "workOrderStatus" = [getOperator operator (workOrder ^. WorkOrder_WorkOrderStatus) (E.val $ T.strip value)]
                                            | otherwise = []

workOrderInPredicate :: E.SqlExpr (Entity WorkOrder_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
workOrderInPredicate workOrder Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                              | T.strip field == "workOrderCode" = [(workOrder ^. WorkOrder_WorkOrderCode) `in_` (E.valList $ fromText P.id value)]
                                              | T.strip field == "workOrderStatus" = [(workOrder ^. WorkOrder_WorkOrderStatus) `in_` (E.valList $ fromText P.id value)]
                                              | otherwise = []

workOrderNotInPredicate :: E.SqlExpr (Entity WorkOrder_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
workOrderNotInPredicate workOrder Predicate {..} | T.strip operator /= "not in" || T.strip value == "" = []
                                                 | T.strip field == "workOrderCode" = [(workOrder ^. WorkOrder_WorkOrderCode) `notIn` (E.valList $ fromText P.id value)]
                                                 | T.strip field == "workOrderStatus" = [(workOrder ^. WorkOrder_WorkOrderStatus) `notIn` (E.valList $ fromText P.id value)]
                                                 | otherwise = []

workOrderPredicates :: E.SqlExpr (Entity WorkOrder_) -> [Predicate] -> [[E.SqlExpr (E.Value Bool)]]
workOrderPredicates _ [] = []
workOrderPredicates workOrder (x:xs) | P.length p == 0 = workOrderPredicates workOrder xs
                                     | otherwise = p : workOrderPredicates workOrder xs
                   where
                      p = (workOrderPredicate workOrder x) P.++ (workOrderInPredicate workOrder x) P.++ (workOrderNotInPredicate workOrder x)

workOrderFilters :: Monad m => E.SqlExpr (Entity WorkOrder_) -> PageArg -> m (E.SqlExpr (E.Value Bool))
workOrderFilters workOrder PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ workOrderPredicates workOrder justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (workOrder ^. WorkOrder_Id E.==. workOrder ^. WorkOrder_Id)
                            return predicates_

workOrderQueryCount :: PageArg -> Handler Int
workOrderQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \ workOrder -> do
                                        filters <- workOrderFilters workOrder page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

workOrderQuery :: PageArg -> Handler [Entity WorkOrder_]
workOrderQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ workOrder -> do
                                        woFilters <- workOrderFilters workOrder page
                                        E.where_ woFilters
                                        E.orderBy [E.asc (workOrder ^. WorkOrder_Id)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return workOrder
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

createUpdateWorkOrderPersistent :: WorkOrderArg -> Handler WorkOrder_Id
createUpdateWorkOrderPersistent arg = do
                let WorkOrderArg {..} = arg
                now <- liftIO getCurrentTime
                randomCode <- liftIO $ randomAlphaNumText 6
                entityId <- if workOrderId > 0 then
                                do
                                  let workOrderKey = (toSqlKey $ fromIntegral $ workOrderId)::WorkOrder_Id
                                  _ <- runDB $ update workOrderKey [ WorkOrder_EstimateDuration =. estimateDuration
                                                                   , WorkOrder_Rate =. rate
                                                                   , WorkOrder_Notes =. notes
                                                                   , WorkOrder_GeneratedById =. ((toSqlKey $ fromIntegral $ generatedById)::Person_Id)
                                                                   , WorkOrder_ResponsibleId =. ((toSqlKey $ fromIntegral $ responsibleId)::Person_Id)
                                                                   , WorkOrder_ParentId =. (case parentId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::WorkOrder_Id))
                                                                   , WorkOrder_ModifiedDate =. Just now
                                                                   ]
                                  return workOrderKey
                               else do
                                  workOrderKey <- runDB $ insert $ fromWorkOrderQL arg now Nothing randomCode
                                  return workOrderKey
                _ <- saveWorkOrderResource resources
                _ <- runDB $ updateWhere  [WorkQueue_Id <-. P.map (\wqId -> (toSqlKey $ fromIntegral $ wqId)) workQueueIds] [WorkQueue_WorkOrderId =. Just entityId, WorkQueue_Status =. "WO_CREATED"]
                return entityId

saveWorkOrderResource :: [WorkOrderResourceArg] -> Handler [WorkOrderResource_Id]
saveWorkOrderResource [] = pure []
saveWorkOrderResource (x:xs) = do
                                  resourceId <-  createUpdateWorkOrderResource x
                                  resourceIds <- saveWorkOrderResource xs
                                  return (resourceId:resourceIds)

createUpdateWorkOrderResource :: WorkOrderResourceArg -> Handler WorkOrderResource_Id
createUpdateWorkOrderResource resource = do
                let WorkOrderResourceArg {..} = resource
                now <- liftIO getCurrentTime
                entityId <- if workOrderResourceId > 0 then
                                do
                                  let workOrderResourceKey = (toSqlKey $ fromIntegral $ workOrderResourceId)::WorkOrderResource_Id
                                  _ <- runDB $ update workOrderResourceKey [ WorkOrderResource_HumanResourceId =. (case humanResourceId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::Person_Id))
                                                                           , WorkOrderResource_Amount =. amount
                                                                           , WorkOrderResource_InventoryItemId =. (case inventoryItemId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::InventoryItem_Id))
                                                                           , WorkOrderResource_WorkQueueId =. ((toSqlKey $ fromIntegral $ workQueueTaskId)::WorkQueue_Id)
                                                                           , WorkOrderResource_ModifiedDate =. Just now
                                                                          ]
                                  return workOrderResourceKey
                               else do
                                  workOrderResourceKey <- runDB $ insert $ fromWorkOrderResourceQL resource now Nothing
                                  return workOrderResourceKey
                return entityId

createUpdateWorkOrderSubTask :: WorkQueue_Id -> WorkOrderSubTaskArg -> Handler WorkOrderSubTask_Id
createUpdateWorkOrderSubTask workQueueId WorkOrderSubTaskArg{..} = do
                now <- liftIO getCurrentTime
                entityId <- if workOrderSubTaskId > 0 then
                                do
                                  let workOrderSubTaskKey = (toSqlKey $ fromIntegral $ workOrderSubTaskId)::WorkOrderSubTask_Id
                                  _ <- runDB $ update workOrderSubTaskKey [ WorkOrderSubTask_SubTaskId =. ((toSqlKey $ fromIntegral subTaskId)::SubTask_Id)
                                                                          , WorkOrderSubTask_Value =. value
                                                                          , WorkOrderSubTask_WorkQueueId =. workQueueId
                                                                          , WorkOrderSubTask_ModifiedDate =. Just now
                                                                          ]
                                  return workOrderSubTaskKey
                               else do
                                  let newWOSubTask = WorkOrderSubTask_{ workOrderSubTask_SubTaskId = (toSqlKey $ fromIntegral subTaskId)::SubTask_Id
                                                                     , workOrderSubTask_Value = value
                                                                     , workOrderSubTask_WorkQueueId = workQueueId
                                                                     , workOrderSubTask_CreatedDate = now
                                                                     , workOrderSubTask_ModifiedDate = Nothing
                                                                     }
                                  workOrderSubTaskKey <- runDB $ insert newWOSubTask
                                  return workOrderSubTaskKey
                return entityId

changeWorkOrderStatus :: EntityChangeStatusArg -> Handler Bool
changeWorkOrderStatus EntityChangeStatusArg{..} = do
                                          let entityId = P.head entityIds
                                          let workOrderId = (toSqlKey $ fromIntegral $ entityId)::WorkOrder_Id
                                          now <- liftIO getCurrentTime
                                          _ <- runDB $ update workOrderId [WorkOrder_WorkOrderStatus =. status, WorkOrder_ModifiedDate =. Just now]
                                          return True

fromWorkOrderQL :: WorkOrderArg -> UTCTime -> Maybe UTCTime -> Text -> WorkOrder_
fromWorkOrderQL (WorkOrderArg {..}) cd md code = WorkOrder_ { workOrder_WorkOrderCode = code
                                                            , workOrder_WorkOrderStatus = "IN_PROGRESS"
                                                            , workOrder_EstimateDuration = estimateDuration
                                                            , workOrder_ExecutionDuration = 0
                                                            , workOrder_Rate = rate
                                                            , workOrder_TotalCost = 0
                                                            , workOrder_Percentage = 0
                                                            , workOrder_Notes = notes
                                                            , workOrder_GeneratedById = ((toSqlKey $ fromIntegral $ generatedById)::Person_Id)
                                                            , workOrder_ResponsibleId = ((toSqlKey $ fromIntegral $ responsibleId)::Person_Id)
                                                            , workOrder_CanceledById = Nothing
                                                            , workOrder_ParentId = (case parentId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::WorkOrder_Id))
                                                            , workOrder_CloseDate = Nothing
                                                            , workOrder_CreatedDate = cd
                                                            , workOrder_ModifiedDate = md
                                                            }

fromWorkOrderResourceQL :: WorkOrderResourceArg -> UTCTime -> Maybe UTCTime -> WorkOrderResource_
fromWorkOrderResourceQL (WorkOrderResourceArg {..}) cd md = WorkOrderResource_ { workOrderResource_HumanResourceId = (case humanResourceId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::Person_Id))
                                                                               , workOrderResource_Amount = amount
                                                                               , workOrderResource_ResourceType = resourceType
                                                                               , workOrderResource_EmployeeCategoryId = (case employeeCategoryId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::Category_Id))
                                                                               , workOrderResource_ItemId = (case itemId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::Item_Id))
                                                                               , workOrderResource_InventoryItemId =  (case inventoryItemId of Nothing -> Nothing; Just a -> Just ((toSqlKey $ fromIntegral a)::InventoryItem_Id))
                                                                               , workOrderResource_WorkQueueId = (toSqlKey $ fromIntegral workQueueTaskId)::WorkQueue_Id
                                                                               , workOrderResource_CreatedDate = cd
                                                                               , workOrderResource_ModifiedDate = md
                                                                               }
