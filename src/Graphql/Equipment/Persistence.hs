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

module Graphql.Equipment.Persistence (
      queryFilters
    , equipmentQuery
    , createOrUpdateEquipment
    , equipmentQueryCount
    , childrenQueryCount
    , equipmentChildrenQuery
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Data.Maybe (maybeToList, listToMaybe)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Graphql.InventoryDataTypes
import Data.Time
import Graphql.InventoryItem
import Graphql.Item
import Graphql.Category
import Graphql.Equipment.DataTypes

getPredicate item Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                 | T.strip field == "name" = [getOperator operator (item ^. Item_Name) (E.val value)]
                                 | T.strip field == "code" = [getOperator operator (item ^. Item_Code) (E.val $ T.strip value)]
                                 | T.strip field == "status" = [getOperator operator (item ^. Item_Status) (E.val (readEntityStatus $ T.strip value))]
                                 | T.strip field == "partNumber" = [getOperator operator (item ^. Item_PartNumber) (E.val $ Just $ T.strip value)]
                                 | T.strip field == "categoryId" = [getOperator operator (item ^. Item_CategoryId) (E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                 | T.strip field == "itemId" = [getOperator operator (item ^. Item_Id) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                 | otherwise = []

getEquipmentPredicate equipment Predicate {..} | T.strip field == "parentId" && T.strip operator == "=" && T.strip value == "null" = [E.isNothing (equipment ^. Equipment_ParentId)]
                                               | T.strip field == "parentId" && T.strip operator == "=" && T.strip value /= "" = [equipment ^. Equipment_ParentId E.==. E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value)]
                                               | T.strip field == "parentId" && T.strip operator == "!=" && T.strip value /= "" = [equipment ^. Equipment_ParentId E.!=. E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value)]
                                               | otherwise = []

getInPredicate item Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                   | T.strip field == "name" = [(item ^. Item_Name) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "code" = [(item ^. Item_Code) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "status" = [(item ^. Item_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                   | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `in_` (E.valList $ fromText (\e -> Just e) value)]
                                   | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `in_` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                   | otherwise = []

getNotInPredicate item Predicate {..} | T.strip operator /= "not in" || T.strip value == "" = []
                                      | T.strip field == "name" = [(item ^. Item_Name) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "code" = [(item ^. Item_Code) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "status" = [(item ^. Item_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                      | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `notIn` (E.valList $ fromText (\e -> Just e) value)]
                                      | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `notIn` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                      | otherwise = []
getPredicates equipment item [] = []
getPredicates equipment item (x:xs) | P.length p == 0 = getPredicates equipment item xs
                                    | otherwise = p : getPredicates equipment item xs
                   where
                      p = (getEquipmentPredicate equipment x) P.++ (getPredicate item x) P.++ (getInPredicate item x) P.++ (getNotInPredicate item x)


queryFilters equipment item PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getPredicates equipment item justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (item ^. Item_Id E.==. item ^. Item_Id)
                            let searchFilters = case searchString of
                                                  Just s -> [item ^. Item_Code E.==. E.val s, item ^. Item_Name `E.like` (%) ++. E.val s ++. (%)]
                                                  Nothing -> [item ^. Item_Id E.==. item ^. Item_Id]
                            let searchFilters_ = unionFilters searchFilters
                            return (searchFilters_ E.&&. predicates_)

equipmentQueryCount :: PageArg -> Handler Int
equipmentQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        filters <- queryFilters equipment item page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

childrenQueryCount :: Item_Id -> PageArg -> Handler Int
childrenQueryCount parentId page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment, item) -> do
                                        filters <- queryFilters equipment item page
                                        E.where_ (equipment ^. Equipment_ParentId E.==. E.val (Just parentId)
                                                 E.&&. equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                                 E.&&. filters
                                                 )
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

equipmentChildrenQuery :: Item_Id -> PageArg -> Handler [(Entity Equipment_, Entity Item_)]
equipmentChildrenQuery parentId page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment, item) -> do
                                     filters <- queryFilters equipment item page
                                     E.where_ (equipment ^. Equipment_ParentId E.==. E.val (Just parentId)
                                                E.&&. equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                                E.&&. filters
                                              )
                                     E.offset $ pageIndex_ * pageSize_
                                     E.limit pageSize_
                                     return (equipment, item)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

equipmentQuery :: PageArg -> Handler [(Entity Equipment_, Entity Item_)]
equipmentQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        filters <- queryFilters equipment item page
                                        E.where_ filters
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return (equipment, item)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

--createOrUpdateEquipment :: EquipmentArg -> Handler (Equipment MutRes)
createOrUpdateEquipment itemEntityId equipment = do
                let EquipmentArg {..} = equipment
                now <- liftIO getCurrentTime
                entityId <- if equipmentId > 0 then
                                do
                                  let itemId = (toSqlKey $ fromIntegral $ equipmentId) :: Item_Id
                                  let equipmentKey = Equipment_Key {unEquipment_Key  = itemId}
                                  _ <- runDB $ update equipmentKey [ Equipment_Priority =. priority
                                                                   , Equipment_HoursAverageDailyUse =. hoursAverageDailyUse
                                                                   , Equipment_OutOfService =. outOfService
                                                                   , Equipment_PurchaseDate =. case purchaseDate of Nothing -> Nothing; Just pd -> Just (read $ show pd :: UTCTime)
                                                                   , Equipment_ParentId =. case parentId of Nothing -> Nothing; Just p -> Just (toSqlKey $ fromIntegral $ p :: Item_Id)
                                                                   , Equipment_ModifiedDate =. Just now
                                                                   ]
                                  return equipmentKey
                               else do
                                  equipmentKey <- runDB $ insert $ fromEquipmentQL itemEntityId equipment now Nothing
                                  return equipmentKey
                return entityId

fromEquipmentQL :: Item_Id -> EquipmentArg -> UTCTime -> Maybe UTCTime -> Equipment_
fromEquipmentQL itemEntityId (EquipmentArg {..}) cd md = Equipment_ { equipment_ItemId = itemEntityId
                                                                    , equipment_Priority = priority
                                                                    , equipment_HoursAverageDailyUse = hoursAverageDailyUse
                                                                    , equipment_OutOfService = outOfService
                                                                    , equipment_PurchaseDate = case purchaseDate of Nothing -> Nothing; Just pd -> Just (read $ show pd :: UTCTime)
                                                                    , equipment_ParentId = case parentId of Nothing -> Nothing; Just p -> Just (toSqlKey $ fromIntegral $ p :: Item_Id)
                                                                    , equipment_CreatedDate = cd
                                                                    , equipment_ModifiedDate = md
                                                                    }
