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

module Graphql.Maintenance.Persistence (
        createOrUpdateMaintenance
      , equipmentQuery
      , maintenanceQuery
      , maintenanceQueryCount
      , maintenanceFilters
      , availableEquipmentQuery
      , availableEquipmentQueryCount
) where

import Import hiding (union)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
--import Data.Time.Calendar (toGregorian, fromGregorian)
--import Data.Text.Time (parseISODateTime)
--import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_, {-countRows-})
import Database.Esqueleto.PostgreSQL (arrayAggDistinct)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Data.Time ()
import Graphql.Maintenance.DataTypes
import Graphql.Asset.Equipment.Persistence (equipmentQueryFilters)
import Graphql.Maintenance.Task.Persistence (taskQuery)

getMaintenancePredicate :: E.SqlExpr (Entity Maintenance_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getMaintenancePredicate maintenance Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                                   | T.strip field == "name" = [getOperator operator (maintenance ^. Maintenance_Name) (E.val value)]
                                                   | T.strip field == "status" = [getOperator operator (maintenance ^. Maintenance_Status) (E.val (readEntityStatus $ T.strip value))]
                                                   | otherwise = []

getMaintenanceInPredicate :: E.SqlExpr (Entity Maintenance_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getMaintenanceInPredicate maintenance Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                                     | T.strip field == "name" = [(maintenance ^. Maintenance_Name) `in_` (E.valList $ fromText P.id value)]
                                                     | T.strip field == "status" = [(maintenance ^. Maintenance_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                                     | otherwise = []

getMaintenanceNotInPredicate :: E.SqlExpr (Entity Maintenance_) -> Predicate -> [E.SqlExpr (E.Value Bool)]
getMaintenanceNotInPredicate maintenance Predicate {..} | T.strip operator /= "notIn" || T.strip value == "" = []
                                                        | T.strip field == "name" = [(maintenance ^. Maintenance_Name) `notIn` (E.valList $ fromText P.id value)]
                                                        | T.strip field == "status" = [(maintenance ^. Maintenance_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                                        | otherwise = []

getMaintenancePredicates :: E.SqlExpr (Entity Maintenance_) -> [Predicate] -> [[E.SqlExpr (E.Value Bool)]]
getMaintenancePredicates _ [] = []
getMaintenancePredicates maintenance (x:xs) | P.length p == 0 = getMaintenancePredicates maintenance xs
                                            | otherwise = p : getMaintenancePredicates maintenance xs
                   where
                      p = (getMaintenancePredicate maintenance x) P.++ (getMaintenanceInPredicate maintenance x) P.++ (getMaintenanceNotInPredicate maintenance x)

maintenanceFilters :: Monad m => E.SqlExpr (Entity Maintenance_) -> PageArg -> m (E.SqlExpr (E.Value Bool))
maintenanceFilters maintenance PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getMaintenancePredicates maintenance justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (maintenance ^. Maintenance_Id E.==. maintenance ^. Maintenance_Id)
                            let searchFilters = case searchString of
                                                  Just s -> [maintenance ^. Maintenance_Name `E.like` (%) ++. E.val s ++. (%)]
                                                  Nothing -> [maintenance ^. Maintenance_Id E.==. maintenance ^. Maintenance_Id]
                            let searchFilters_ = unionFilters searchFilters
                            return (searchFilters_ E.&&. predicates_)

maintenanceQueryCount :: PageArg -> Handler Int
maintenanceQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \ maintenance -> do
                                        filters <- maintenanceFilters maintenance page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

maintenanceQuery :: PageArg -> Handler [Entity Maintenance_]
maintenanceQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ maintenance -> do
                                        mFilters <- maintenanceFilters maintenance page
                                        E.where_ mFilters
                                        E.orderBy [E.asc (maintenance ^. Maintenance_Id)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return maintenance
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

equipmentQuery :: Maintenance_Id -> Handler [(Entity Equipment_, Entity Item_)]
equipmentQuery maintenanceId =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
--                                        let subquery =
--                                              E.from $ \workQueue -> do
--                                              E.where_ (workQueue ^. WorkQueue_MaintenanceId E.==. E.val (Just maintenanceId))
--                                              return (workQueue ^. WorkQueue_EquipmentId)
--                                        E.where_ (equipment ^. Equipment_ItemId `E.in_` E.subList_select subquery)
                                        E.where_ (equipment ^. Equipment_MaintenanceId E.==. E.val (Just maintenanceId))
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        return (equipment, item)
                      return result

availableEquipmentQueryCount :: PageArg -> Handler Int
availableEquipmentQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
--                                        let subquery =
--                                              E.from $ \workQueue -> do
--                                              E.where_ (workQueue ^. WorkQueue_Status E.!=. E.val DELETED)
--                                              return (workQueue ^. WorkQueue_EquipmentId)
                                        filters <- equipmentQueryFilters equipment item page
--                                        E.where_ (filters E.&&. equipment ^. Equipment_ItemId `E.notIn` E.subList_select subquery)
                                        E.where_ (filters E.&&. E.isNothing (equipment ^. Equipment_MaintenanceId))
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

availableEquipmentQuery :: PageArg -> Handler [(Entity Equipment_, Entity Item_)]
availableEquipmentQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        let subquery =
                                              E.from $ \workQueue -> do
                                              E.where_ (workQueue ^. WorkQueue_Status E.!=. E.val "DELETED")
                                              return (workQueue ^. WorkQueue_EquipmentId)
                                        eFilters <- equipmentQueryFilters equipment item page
                                        E.where_ (eFilters E.&&. equipment ^. Equipment_ItemId `E.notIn` E.subList_select subquery)
                                        E.orderBy [E.asc (equipment ^. Equipment_ItemId)]
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return (equipment, item)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

createOrUpdateMaintenance :: MaintenanceArg -> Handler Maintenance_Id
createOrUpdateMaintenance maintenance = do
                let MaintenanceArg {..} = maintenance
                now <- liftIO getCurrentTime
                entityId <- if maintenanceId > 0 then
                                do
                                  let maintenanceKey = (toSqlKey $ fromIntegral $ maintenanceId)::Maintenance_Id
                                  _ <- runDB $ update maintenanceKey [ Maintenance_Name =. name
                                                                     , Maintenance_Description =. description
                                                                     , Maintenance_Status =. readEntityStatus status
                                                                     , Maintenance_ModifiedDate =. Just now
                                                                     ]
                                  return maintenanceKey
                               else do
                                  maintenanceKey <- runDB $ insert $ fromMaintenanceQL maintenance now Nothing
                                  return maintenanceKey
                return entityId

fromMaintenanceQL :: MaintenanceArg -> UTCTime -> Maybe UTCTime -> Maintenance_
fromMaintenanceQL (MaintenanceArg {..}) cd md = Maintenance_ { maintenance_Name = name
                                                             , maintenance_Description = description
                                                             , maintenance_Status = readEntityStatus status
                                                             , maintenance_CreatedDate = cd
                                                             , maintenance_ModifiedDate = md
                                                             }
