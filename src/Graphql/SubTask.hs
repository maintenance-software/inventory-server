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

module Graphql.SubTask (
      subTaskResolver_
    , getSubTaskByIdResolver
    , saveSubTasks
    , toSubTaskQL
    , SubTask
    , SubTaskArg
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
import Graphql.SubTaskKind

data SubTask o = SubTask { subTaskId :: Int
                         , order :: Int
                   	     , group :: Text
                   	     , description :: Maybe Text
                   	     , mandatory :: Bool
                         , createdDate :: Text
                         , modifiedDate :: Maybe Text
                   	     , subTaskKind :: Maybe(() -> o () Handler SubTaskKind)                   
                         } deriving (Generic, GQLType)

data SubTaskArg = SubTaskArg { subTaskId :: Int
                             , order :: Int
                             , group :: Text
                             , description :: Maybe Text
                             , mandatory :: Bool
                             , subTaskKindId :: Maybe Int
                             } deriving (Generic)

instance GQLType SubTaskArg where
    type  KIND SubTaskArg = INPUT_OBJECT
    description = const $ Just $ pack "This field holds SubTask Input information"

subTaskQuery :: Task_Id -> Handler [Entity SubTask_]
subTaskQuery taskId =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \subTask -> do
                                        E.where_ (subTask ^. SubTask_TaskId E.==. E.val taskId)
                                        return subTask
                      return result

subTaskResolver_ taskId _ = lift $ do
                                subTasks <- subTaskQuery taskId
                                return $ P.map (\t -> toSubTaskQL t) subTasks

--getSubTaskByIdResolver :: GetEntityByIdArg -> Res e Handler (SubTask Res)
getSubTaskByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let subTaskId = (toSqlKey $ fromIntegral $ entityId)::SubTask_Id
                                              subTask <- runDB $ getJustEntity subTaskId
                                              return $ toSubTaskQL subTask


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

-- CONVERTERS
--toSubTaskQL :: Entity SubTask_ -> SubTask
toSubTaskQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity SubTask_ -> SubTask o
toSubTaskQL (Entity subTaskId subTask) = SubTask { subTaskId = fromIntegral $ fromSqlKey subTaskId
                                                 , order = subTask_Order
                                                 , group = subTask_Group
                                                 , description = subTask_Description
                                                 , mandatory = subTask_Mandatory
                                                 , subTaskKind = case subTask_SubTaskKindId of Nothing -> Nothing; Just c -> Just $ getSubTaskKindByIdResolver_ c
                                                 , createdDate = fromString $ show subTask_CreatedDate
                                                 , modifiedDate = m
                                                 }
                                          where
                                            SubTask_ {..} = subTask
                                            m = case subTask_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

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

{-
query {
  inventories(queryString: "") {
    subTaskId
    name
    description
  }
}

mutation {
  saveCategory(subTaskId: 0, name: "test", description: "sss") {
    subTaskId
    name
  }
}
-}
