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

module Graphql.Maintenance.TaskTrigger.Resolvers (
      taskTriggerResolver_
    , getTaskTriggerByIdResolver
    , getTaskTriggerByIdResolver_
    , toTaskTriggerQL
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Graphql.Utils hiding(unionFilters, conjunctionFilters, getOperator)
import Graphql.Maintenance.SubTask.Resolvers ()
import Graphql.Maintenance.TaskTrigger.DataTypes
import Graphql.Maintenance.TaskTrigger.Persistence
import Graphql.Asset.Unit
import Graphql.Category

taskTriggerResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => Task_Id -> p -> t (HandlerFor App) [TaskTrigger o]
taskTriggerResolver_ taskId _ = lift $ do
                                tasks <- taskTriggerQuery taskId
                                return $ P.map (\t -> toTaskTriggerQL t) tasks

getTaskTriggerByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (TaskTrigger o)
getTaskTriggerByIdResolver EntityIdArg {..} = lift $ do
                                              let taskTriggerId = (toSqlKey $ fromIntegral $ entityId)::TaskTrigger_Id
                                              taskTrigger <- runDB $ getJustEntity taskTriggerId
                                              return $ toTaskTriggerQL taskTrigger

getTaskTriggerByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => TaskTrigger_Id -> () -> o () Handler (TaskTrigger o)
getTaskTriggerByIdResolver_ taskTriggerId _ = lift $ do
                                              taskTrigger <- runDB $ getJustEntity taskTriggerId
                                              return $ toTaskTriggerQL taskTrigger

-- CONVERTERS
toTaskTriggerQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity TaskTrigger_ -> TaskTrigger o
toTaskTriggerQL (Entity taskTriggerId taskTrigger) = TaskTrigger { taskTriggerId = fromIntegral $ fromSqlKey taskTriggerId
                                                                 , triggerType = taskTrigger_TriggerType
                                                                 , description = taskTrigger_Description
                                                                 , fixedSchedule = taskTrigger_FixedSchedule
                                                                 , frequency = taskTrigger_Frequency
                                                                 , readType = taskTrigger_ReadType
                                                                 , limit = taskTrigger_Limit
                                                                 , repeat = taskTrigger_Repeat
                                                                 , operator = taskTrigger_Operator
                                                                 , value = taskTrigger_Value
                                                                 , timeFrequency = (case taskTrigger_TimeFrequency of Nothing -> Nothing; Just t -> Just $ T.pack $ show t)
                                                                 , unit = case taskTrigger_UnitId of Nothing -> Nothing; Just c -> Just $ getUnitByIdResolver_ c
                                                                 , eventTriggerCategory = case taskTrigger_EventTriggerCategoryId of Nothing -> Nothing; Just c -> Just $ getCategoryByIdResolver_ c
                                                                 , createdDate = fromString $ show taskTrigger_CreatedDate
                                                                 , modifiedDate = m
                                                                 }
                                          where
                                            TaskTrigger_ {..} = taskTrigger
                                            m = case taskTrigger_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

{-
query {
  inventories(queryString: "") {
    taskId
    name
    description
  }
}

mutation {
  saveCategory(taskId: 0, name: "test", description: "sss") {
    taskId
    name
  }
}
-}
