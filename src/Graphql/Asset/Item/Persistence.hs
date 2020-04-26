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

module Graphql.Asset.Item.Persistence (
      itemQueryCount
    , itemQuery
    , availableItemsQueryCount
    , availableItemsQuery
    , changeStatus
    , createOrUpdateItem
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Data.Text as T
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Data.Typeable (typeOf)
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Data.Time
import Enums
import Graphql.Asset.Unit
import Graphql.Asset.DataTypes
--getFilters Nothing = []
--getFilters (Just []) = []
--getFilters (Just (x:xs)) | T.strip field == "" || T.strip operator == "" || T.strip value == ""  = getFilters $ Just xs
--                         | T.strip field == "status" = ((getOperator operator) Item_Status (readEntityStatus $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "itemType" = ((getOperator operator) Item_ItemType (readItemType $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "name" = ((getOperator operator) Item_Name (T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "partNumber" = ((getOperator operator) Item_PartNumber (Just $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "manufacturer" = ((getOperator operator) Item_Manufacturer (Just $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "model" = ((getOperator operator) Item_Model (Just $ T.strip value)) : (getFilters $ Just xs)
--                         | T.strip field == "categoryId" = ((getOperator operator) Item_CategoryId (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value)) : (getFilters $ Just xs)
--                         | otherwise = getFilters $ Just xs
--                   where
--                      Predicate {..} = x

--getPredicate Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
--                            | T.strip field == "status" = [((getOperator operator) Item_Status (readEntityStatus $ T.strip value))]
--                            | T.strip field == "itemType" = [((getOperator operator) Item_ItemType (readItemType $ T.strip value))]
--                            | T.strip field == "name" = [((getOperator operator) Item_Name (T.strip value))]
--                            | T.strip field == "partNumber" = [((getOperator operator) Item_PartNumber (Just $ T.strip value))]
--                            | T.strip field == "manufacturer" = [((getOperator operator) Item_Manufacturer (Just $ T.strip value))]
--                            | T.strip field == "model" = [((getOperator operator) Item_Model (Just $ T.strip value))]
--                            | T.strip field == "categoryId" = [((getOperator operator) Item_CategoryId (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
--                            | otherwise = []
--
--getInPredicate Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
--                              | T.strip field == "status" = [Item_Status <-. textToList value readEntityStatus]
--                              | T.strip field == "itemType" = [Item_ItemType <-. textToList value readItemType]
--                              | T.strip field == "name" = [Item_Name <-. textToList value P.id]
--                              | T.strip field == "partNumber" = [Item_PartNumber <-. textToList value Just]
--                              | T.strip field == "manufacturer" = [Item_Manufacturer <-. textToList value Just]
--                              | T.strip field == "model" = [Item_Model <-. textToList value Just]
--                              | T.strip field == "categoryId" = [Item_CategoryId <-. textToList value (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e)]
--                              | otherwise = []

getPredicate item Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                 | T.strip field == "name" = [getOperator operator (item ^. Item_Name) (E.val value)]
                                 | T.strip field == "code" = [getOperator operator (item ^. Item_Code) (E.val $ T.strip value)]
                                 | T.strip field == "status" = [getOperator operator (item ^. Item_Status) (E.val (readEntityStatus $ T.strip value))]
                                 | T.strip field == "itemType" = [getOperator operator (item ^. Item_ItemType) (E.val (readItemType $ T.strip value))]
                                 | T.strip field == "partNumber" = [getOperator operator (item ^. Item_PartNumber) (E.val $ Just $ T.strip value)]
                                 | T.strip field == "categoryId" = [getOperator operator (item ^. Item_CategoryId) (E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                 | T.strip field == "itemId" = [getOperator operator (item ^. Item_Id) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                 | otherwise = []

getInPredicate item Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                   | T.strip field == "name" = [(item ^. Item_Name) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "code" = [(item ^. Item_Code) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "status" = [(item ^. Item_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                   | T.strip field == "itemType" = [(item ^. Item_ItemType) `in_` (E.valList $ fromText readItemType value)]
                                   | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `in_` (E.valList $ fromText (\e -> Just e) value)]
                                   | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `in_` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                   | otherwise = []

getNotInPredicate item Predicate {..} | T.strip operator /= "not in" || T.strip value == "" = []
                                      | T.strip field == "name" = [(item ^. Item_Name) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "code" = [(item ^. Item_Code) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "status" = [(item ^. Item_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                      | T.strip field == "itemType" = [(item ^. Item_ItemType) `notIn` (E.valList $ fromText readItemType value)]
                                      | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `notIn` (E.valList $ fromText (\e -> Just e) value)]
                                      | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `notIn` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                      | otherwise = []

getPredicates _ [] = []
getPredicates item (x:xs) | P.length p == 0 = getPredicates item xs
                          | otherwise = p : getPredicates item xs
                   where
                      p = (getPredicate item x) P.++ (getInPredicate item x) P.++ (getNotInPredicate item x)

itemFilters item PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getPredicates item justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (item ^. Item_Id E.==. item ^. Item_Id)
                            let searchFilters = case searchString of
                                                  Just s -> [item ^. Item_Code E.==. E.val s, item ^. Item_Name `E.like` (%) ++. E.val s ++. (%)]
                                                  Nothing -> [item ^. Item_Id E.==. item ^. Item_Id]
                            let searchFilters_ = unionFilters searchFilters
                            return (searchFilters_ E.&&. predicates_)

-- QUERIES
itemQueryCount :: PageArg -> Handler Int
itemQueryCount page =  do
                      result  <- runDB
                                   $ E.select
                                   $ E.from $ \ item -> do
                                        filters <- itemFilters item page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ result

itemQuery :: PageArg -> Handler [Entity Item_]
itemQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \ item -> do
                                        filters <- itemFilters item page
                                        E.where_ filters
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return item
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

availableItemsQueryCount :: Inventory_Id -> PageArg -> Handler Int
availableItemsQueryCount inventoryId page = do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \item -> do
                                        let subquery =
                                              E.from $ \inventoryItem -> do
                                              E.where_ (inventoryItem ^. InventoryItem_InventoryId E.==. E.val inventoryId)
                                              return (inventoryItem ^. InventoryItem_ItemId)
                                        filters <- itemFilters item page
                                        E.where_ (item ^. Item_Id `E.notIn` (E.subList_select subquery) E.&&. filters)
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ result

availableItemsQuery :: Inventory_Id -> PageArg -> Handler [Entity Item_]
availableItemsQuery inventoryId page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \item -> do
                                        let subquery =
                                              E.from $ \inventoryItem -> do
                                              E.where_ (inventoryItem ^. InventoryItem_InventoryId E.==. E.val inventoryId)
                                              return (inventoryItem ^. InventoryItem_ItemId)
                                        filters <- itemFilters item page
                                        E.where_ (item ^. Item_Id `E.notIn` E.subList_select subquery E.&&. filters)
                                        return item
                      return result
-- END QUERIES

changeStatus :: [Int] -> EntityStatus -> Handler ()
changeStatus [] _ = pure ()
changeStatus (x:xs) status = do
                        let itemId = (toSqlKey $ fromIntegral $ x)::Item_Id
                        now <- liftIO getCurrentTime
                        _ <- runDB $ update itemId [ Item_Status =. status, Item_ModifiedDate =. Just now]
                        _ <- changeStatus xs status
                        return ()

createOrUpdateItem :: ItemArg -> Handler Item_Id
createOrUpdateItem item = do
                            let ItemArg {..} = item
                            now <- liftIO getCurrentTime
                            itemEntityId <- if itemId > 0 then
                                        do
                                         let itemKey = (toSqlKey $ fromIntegral $ itemId)::Item_Id
                                         _ <- runDB $ update itemKey [ Item_Code =. code
                                                                     , Item_Name =. name
                                                                     , Item_DefaultPrice =. realToFrac defaultPrice
                                                                     , Item_Description =. description
                                                                     , Item_PartNumber =. partNumber
                                                                     , Item_Manufacturer =. manufacturer
                                                                     , Item_Model =. model
                                                                     , Item_ItemType =. readItemType itemType
                                                                     , Item_Notes =. notes
                                                                     , Item_Status =. readEntityStatus status
                                                                     , Item_Images =. images
                                                                     , Item_CategoryId =. case categoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Category_Id)
                                                                     , Item_UnitId =. case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                                                     , Item_ModifiedDate =. Just now
                                                                     ]
                                         return itemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromItemQL item now Nothing
                                            return itemKey
                            return itemEntityId

fromItemQL :: ItemArg -> UTCTime -> Maybe UTCTime -> Item_
fromItemQL (ItemArg {..}) cd md = Item_ { item_Code = code
                                        , item_Name = name
                                        , item_DefaultPrice = realToFrac defaultPrice
                                        , item_Description = description
                                        , item_PartNumber = partNumber
                                        , item_Manufacturer = manufacturer
                                        , item_Model = model
                                        , item_ItemType = readItemType itemType
                                        , item_Notes = notes
                                        , item_Status = readEntityStatus status
                                        , item_Images = images
                                        , item_CategoryId = case categoryId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Category_Id)
                                        , item_UnitId = case unitId of Nothing -> Nothing; Just c -> Just ((toSqlKey $ fromIntegral $ c)::Unit_Id)
                                        , item_CreatedDate = cd
                                        , item_ModifiedDate = md
                                        }

