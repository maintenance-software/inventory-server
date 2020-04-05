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

module Graphql.Item (
        itemResolver
      , getItemByIdResolver_
      , saveItemResolver
      , createOrUpdateItem
      , toItemQL
      , availableItemsPageResolver_
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
import Graphql.Category
import Graphql.InventoryDataTypes
import Enums
import Graphql.InventoryItem
import Graphql.Unit

-- Query Resolvers
--getItemByIdResolver :: GetEntityByIdArg -> Res e Handler (Item Res)
getItemByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => GetEntityByIdArg -> o () Handler (Item o)
getItemByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let itemId = (toSqlKey $ fromIntegral $ entityId)::Item_Id
                                              item <- runDB $ getJustEntity itemId
                                              return $ toItemQL item

getItemByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Item_Id -> () -> o () Handler (Item o)
getItemByIdResolver_ itemId _ = lift $ do
                                         item <- runDB $ getJustEntity itemId
                                         return $ toItemQL item

categoryResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Category_Id -> () -> o () Handler Category
categoryResolver_ categoryId arg = lift $ do
                                      category <- dbFetchCategoryById categoryId
                                      return category

getUnitByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Unit_Id -> () -> o () Handler Unit
getUnitByIdResolver_ unitId _ = lift $ do
                                      unit <- dbFetchUnitById unitId
                                      return unit

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
                                 | T.strip field == "partNumber" = [getOperator operator (item ^. Item_PartNumber) (E.val $ Just $ T.strip value)]
                                 | T.strip field == "categoryId" = [getOperator operator (item ^. Item_CategoryId) (E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                 | T.strip field == "itemId" = [getOperator operator (item ^. Item_Id) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
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

--itemsPageResolver :: PageArg -> Res e Handler (Page (Item Res))
itemsPageResolver page = lift $ do
                        countItems <- itemQueryCount page
                        result <- itemQuery page
                        let itemsQL = P.map (\r -> toItemQL r) result
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                          , hasPreview = pageIndex' * pageSize' > 0
                                                          , pageSize = pageSize'
                                                          , pageIndex = pageIndex'
                                    }
                        }
                         where
                          PageArg {..} = page
                          pageIndex' = case pageIndex of
                                        Just  x  -> x
                                        Nothing -> 0
                          pageSize' = case pageSize of
                                          Just y -> y
                                          Nothing -> 10

availableItemsPageResolver_ inventoryId page = lift $ do
                        countItems <- availableItemsQueryCount inventoryId page
                        result <- availableItemsQuery inventoryId page
                        let itemsQL = P.map (\r -> toItemQL r) result
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                          , hasPreview = pageIndex' * pageSize' > 0
                                                          , pageSize = pageSize'
                                                          , pageIndex = pageIndex'
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex' = case pageIndex of
                                          Just  x  -> x
                                          Nothing -> 0
                            pageSize' = case pageSize of
                                            Just y -> y
                                            Nothing -> 10

--itemResolver :: () -> Res e Handler Items
itemResolver _ = pure Items { item = getItemByIdResolver
                            , page = itemsPageResolver
                            , saveItem = saveItemResolver
                            , changeItemStatus = changeItemStatusResolver
                            }

-- itemResolver :: Items (Res () Handler)
-- itemResolver = Items {  item = getItemByIdResolver, page = itemsPageResolver }

-- categoryResolver :: Category_Id -> () -> Res e Handler Category
--categoryResolver categoryId arg = lift $ do
--                                      category <- dbFetchCategoryById categoryId
--                                      return category

--getUnitByIdResolver_ unitId _ = lift $ do
--                                      unit <- dbFetchUnitById unitId
--                                      return unit

toItemQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Item_ -> Item o
toItemQL (Entity itemId item) = Item { itemId = fromIntegral $ fromSqlKey itemId
                                     , code = item_Code
                                     , name = item_Name
                                     , defaultPrice = realToFrac item_DefaultPrice
                                     , description = item_Description
                                     , partNumber = item_PartNumber
                                     , manufacturer = item_Manufacturer
                                     , model = item_Model
                                     , itemType = T.pack $ show item_ItemType
                                     , notes = item_Notes
                                     , status = T.pack $ show item_Status
                                     , images = item_Images
                                     , category = case item_CategoryId of Nothing -> Nothing; Just c -> Just $ categoryResolver_ c
                                     , unit = case item_UnitId of Nothing -> Nothing; Just u -> Just $ getUnitByIdResolver_ u
                                     , inventoryItems = inventoryItemsItemPageResolver_ itemId
                                     , createdDate = fromString $ show item_CreatedDate
                                     , modifiedDate = m
                                     }
                            where
                              Item_ {..} = item
                              m = case item_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing

-- Mutation Resolvers
--changeItemStatusResolver :: EntityChangeStatusArg -> MutRes e Handler Bool
changeItemStatusResolver EntityChangeStatusArg {..} = lift $ do
                              () <- changeStatus entityIds (readEntityStatus status)
                              return True
changeStatus :: [Int] -> EntityStatus -> Handler ()
changeStatus [] _ = pure ()
changeStatus (x:xs) status = do
                        let itemId = (toSqlKey $ fromIntegral $ x)::Item_Id
                        now <- liftIO getCurrentTime
                        _ <- runDB $ update itemId [ Item_Status =. status, Item_ModifiedDate =. Just now]
                        _ <- changeStatus xs status
                        return ()

--saveItemResolver :: ItemArg -> MutRes e Handler (Item MutRes)
saveItemResolver arg = lift $ do
                              itemId <- createOrUpdateItem arg
                              item <- runDB $ getJustEntity itemId
                              return $ toItemQL item

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

{-
query {
  items {
    page(pageIndex:0, pageSize: 10) {
      totalCount
      pageInfo {
        pageIndex
        pageSize
        hasNext
        hasPreview
      }
      content {
        itemId
        name
        unit
        defaultPrice
        description
        code
        images
        createdDate
        modifiedDate
        category {
          categoryId
          name
        }
      }

    }
  }
}

mutation {
  saveRole(itemId:10, key: "test12", name: "sloss", description: "option" active: true) {
    itemId
    key
    description
    active
    createdDate
    modifiedDate
    privileges(entityIds: [16]) {
      privilegeId
      key
      description
      active
      createdDate
      modifiedDate
    }
  }
}

-}
