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

module Graphql.Maintenance.SubTask.Resolvers (
      subTaskResolver_
    , getSubTaskByIdResolver
    , getSubTaskByIdResolver_
) where

import Import
import Data.Morpheus.Types (lift)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils hiding(unionFilters, conjunctionFilters, getOperator)
import Graphql.Category
import Graphql.Maintenance.SubTask.Persistence
import Graphql.Maintenance.SubTask.DataTypes

subTaskResolver_ taskId _ = lift $ do
                                subTasks <- subTaskQuery taskId
                                return $ P.map (\t -> toSubTaskQL t) subTasks

getSubTaskByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (SubTask o)
getSubTaskByIdResolver EntityIdArg {..} = lift $ do
                                              let subTaskId = (toSqlKey $ fromIntegral $ entityId)::SubTask_Id
                                              subTask <- runDB $ getJustEntity subTaskId
                                              return $ toSubTaskQL subTask

getSubTaskByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => SubTask_Id -> () -> o () Handler (SubTask o)
getSubTaskByIdResolver_ subTaskId _ = lift $ do
                                              subTask <- runDB $ getJustEntity subTaskId
                                              return $ toSubTaskQL subTask

-- CONVERTERS
--toSubTaskQL :: Entity SubTask_ -> SubTask
toSubTaskQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity SubTask_ -> SubTask o
toSubTaskQL (Entity subTaskId subTask) = SubTask { subTaskId = fromIntegral $ fromSqlKey subTaskId
                                                 , order = subTask_Order
                                                 , group = subTask_Group
                                                 , description = subTask_Description
                                                 , mandatory = subTask_Mandatory
                                                 , subTaskCategory = case subTask_SubTaskCategoryId of Nothing -> Nothing; Just c -> Just $ getCategoryByIdResolver_ c
                                                 , createdDate = fromString $ show subTask_CreatedDate
                                                 , modifiedDate = m
                                                 }
                                          where
                                            SubTask_ {..} = subTask
                                            m = case subTask_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing
