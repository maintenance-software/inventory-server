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

module Graphql.Maintenance.Task.TaskResource (
      TaskResource
    , TaskResourceArg
    , getTaskResourceByIdResolver_
    , fetchTaskResourceResolver_
    , saveTaskResources
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Graphql.Asset.DataTypes (Item(..))
import Graphql.Asset.Item.Resolvers (getItemByIdResolver_)
import Graphql.Asset.Human.DataTypes (Employee(..))
import Graphql.Asset.Human.Resolvers (getEmployeeByIdResolver_)
import Graphql.Category
import Graphql.Asset.Unit (Unit(..), getUnitByIdResolver_)
import Data.Time

data TaskResource o = TaskResource { taskResourceId :: Int
                                   , order :: Int
                                   , amount :: Int
                                   , resourceType :: Text
                                   , unit :: () -> o () Handler Unit
                                   , employeeCategory :: Maybe(() -> o () Handler Category)
                                   , humanResource :: Maybe(() -> o () Handler (Employee o))
                                   , inventoryResource :: Maybe(() -> o () Handler (Item o))
                                   , createdDate :: Text
                                   , modifiedDate :: Maybe Text
                                   } deriving (Generic, GQLType)

data TaskResourceArg = TaskResourceArg { taskResourceId :: Int
                                       , order :: Int
                                       , amount :: Int
                                       , resourceType :: Text
                                       , unitId :: Int
                                       , employeeCategoryId :: Maybe Int
                                       , humanResourceId :: Maybe Int
                                       , inventoryResourceId :: Maybe Int
                                       } deriving (Generic)

instance GQLType TaskResourceArg where
    type  KIND TaskResourceArg = INPUT_OBJECT
    description = const $ Just $ pack "This field holds TaskResource Input information"


getTaskResourceByIdResolver_ taskResourceId arg = lift $ do
                                      taskResource <- runDB $ getJustEntity taskResourceId
                                      return $ toTaskResourceQL taskResource

--fetchTaskResourceResolver_ :: Task_Id -> () -> Res e Handler [TaskResource Res]
fetchTaskResourceResolver_ taskId _ = lift $ do
                               taskResources <- runDB $ selectList [TaskResource_TaskId ==. taskId] []
                               return $ P.map toTaskResourceQL taskResources

--saveTaskResourceResolver :: Task_Id -> TaskResourceArg -> MutRes e Handler TaskResource
--saveTaskResourceResolver taskId arg = lift $ createOrUpdateTaskResource arg

saveTaskResources _ [] = pure []
saveTaskResources taskId (x:xs) = do
                                  taskResourceId <-  createOrUpdateTaskResource taskId x
                                  taskResourceIds <- saveTaskResources taskId xs
                                  return (taskResourceId:taskResourceIds)

--createOrUpdateTaskResource :: Task_Id -> TaskResourceArg -> Handler TaskResource_Id
createOrUpdateTaskResource taskId taskResourceArg = do
                          let TaskResourceArg {..} = taskResourceArg
                          now <- liftIO getCurrentTime
                          entityId <- if taskResourceId > 0 then
                                          do
                                            let taskResourceKey = (toSqlKey $ fromIntegral $ taskResourceId)::TaskResource_Id
                                            _ <- runDB $ update taskResourceKey [ TaskResource_Order =. order
                                                                                , TaskResource_Amount =. amount
                                                                                , TaskResource_ResourceType =. resourceType
                                                                                , TaskResource_UnitId =. ((toSqlKey $ fromIntegral $ unitId)::Unit_Id)
                                                                                , TaskResource_EmployeeCategoryId =. (case employeeCategoryId of Nothing -> Nothing; Just k -> Just ((toSqlKey $ fromIntegral $ k)::Category_Id))
                                                                                , TaskResource_HumanResourceId =. (case humanResourceId of Nothing -> Nothing; Just k -> Just ((toSqlKey $ fromIntegral $ k)::Person_Id))
                                                                                , TaskResource_InventoryResourceId =. (case inventoryResourceId of Nothing -> Nothing; Just k -> Just ((toSqlKey $ fromIntegral k)::Item_Id))
                                                                                , TaskResource_TaskId =. taskId
                                                                                , TaskResource_ModifiedDate =. Just now
                                                                                ]
                                            return taskResourceKey
                                         else do
                                            taskResourceKey <- runDB $ insert $ fromTaskResourceQL taskId taskResourceArg now Nothing
                                            return taskResourceKey
                          return $ entityId

--toTaskResourceQL :: Entity TaskResource_ -> TaskResource MutRes
toTaskResourceQL (Entity taskResourceId taskResourceArg) = TaskResource { taskResourceId = fromIntegral $ fromSqlKey taskResourceId
                                                                        , order = taskResource_Order
                                                                        , amount = taskResource_Amount
                                                                        , resourceType = taskResource_ResourceType
                                                                        , unit = getUnitByIdResolver_ taskResource_UnitId
                                                                        , employeeCategory = (case taskResource_EmployeeCategoryId of Nothing ->Nothing; Just k -> Just $ getCategoryByIdResolver_ k)
                                                                        , humanResource = (case taskResource_HumanResourceId of Nothing ->Nothing; Just k -> Just $ getEmployeeByIdResolver_ k)
                                                                        , inventoryResource = (case taskResource_InventoryResourceId of Nothing ->Nothing; Just k -> Just $ getItemByIdResolver_ k)
                                                                        , createdDate = fromString $ show taskResource_CreatedDate
                                                                        , modifiedDate = m
                                                                        }
                          where
                            TaskResource_ {..} = taskResourceArg
                            m = case taskResource_ModifiedDate of
                                  Just d -> Just $ fromString $ show d
                                  Nothing -> Nothing

fromTaskResourceQL :: Task_Id -> TaskResourceArg -> UTCTime -> Maybe UTCTime -> TaskResource_
fromTaskResourceQL taskId (TaskResourceArg {..}) cd md = TaskResource_ { taskResource_Order = order
                                                                , taskResource_Amount = amount
                                                                , taskResource_ResourceType = resourceType
                                                                , taskResource_UnitId = (toSqlKey $ fromIntegral $ unitId)::Unit_Id
                                                                , taskResource_EmployeeCategoryId = (case employeeCategoryId of Nothing -> Nothing; Just k -> Just ((toSqlKey $ fromIntegral $ k)::Category_Id))
                                                                , taskResource_HumanResourceId = (case humanResourceId of Nothing -> Nothing; Just k -> Just ((toSqlKey $ fromIntegral $ k)::Person_Id))
                                                                , taskResource_InventoryResourceId = (case inventoryResourceId of Nothing -> Nothing; Just k -> Just ((toSqlKey $ fromIntegral k)::Item_Id))
                                                                , taskResource_TaskId = taskId
                                                                , taskResource_CreatedDate = cd
                                                                , taskResource_ModifiedDate = md
                                                                }
