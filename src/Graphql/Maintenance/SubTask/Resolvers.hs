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

module Graphql.Maintenance.SubTask.Resolvers (
      subTaskResolver_
    , getSubTaskByIdResolver
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
import Graphql.Maintenance.SubTask.SubTaskKind
import Graphql.Maintenance.SubTask.Persistence
import Graphql.Maintenance.SubTask.DataTypes

subTaskResolver_ taskId _ = lift $ do
                                subTasks <- subTaskQuery taskId
                                return $ P.map (\t -> toSubTaskQL t) subTasks

--getSubTaskByIdResolver :: GetEntityByIdArg -> Res e Handler (SubTask Res)
getSubTaskByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let subTaskId = (toSqlKey $ fromIntegral $ entityId)::SubTask_Id
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
                                                 , subTaskKind = case subTask_SubTaskKindId of Nothing -> Nothing; Just c -> Just $ getSubTaskKindByIdResolver_ c
                                                 , createdDate = fromString $ show subTask_CreatedDate
                                                 , modifiedDate = m
                                                 }
                                          where
                                            SubTask_ {..} = subTask
                                            m = case subTask_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing
