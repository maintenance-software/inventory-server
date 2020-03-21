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

#ifdef ITEM_DEF

-- toItemQL :: Entity Item_ -> (Item Res)
toItemQL (Entity itemId item) = Item { itemId = fromIntegral $ fromSqlKey itemId
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
                                     , category = categoryResolver item_CategoryId
                                     , unit = getUnitByIdResolver_ item_UnitId
                                     , inventoryItems = inventoryItemsItemPageResolver_ itemId
                                     , createdDate = fromString $ show item_CreatedDate
                                     , modifiedDate = m
                                     }
                            where
                              Item_ {..} = item
                              m = case item_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing


--getItemByIdResolver_ :: Item_Id -> ()-> Res e Handler (Item Res)
getItemByIdResolver_ itemId _ = lift $ do
                                         item <- runDB $ getJustEntity itemId
                                         return $ toItemQL item

categoryResolver categoryId arg = lift $ do
                                      category <- dbFetchCategoryById categoryId
                                      return category

getUnitByIdResolver_ unitId _ = lift $ do
                                      unit <- dbFetchUnitById unitId
                                      return unit

#undef ITEM_DEF

#elif 1

module Graphql.Item (
        itemResolver
      , saveItemResolver
      , toItemQL
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Data.Text as T
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
getItemByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let itemId = (toSqlKey $ fromIntegral $ entityId)::Item_Id
                                              item <- runDB $ getJustEntity itemId
                                              return $ toItemQL item

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

getPredicate Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                            | T.strip field == "status" = [((getOperator operator) Item_Status (readEntityStatus $ T.strip value))]
                            | T.strip field == "itemType" = [((getOperator operator) Item_ItemType (readItemType $ T.strip value))]
                            | T.strip field == "name" = [((getOperator operator) Item_Name (T.strip value))]
                            | T.strip field == "partNumber" = [((getOperator operator) Item_PartNumber (Just $ T.strip value))]
                            | T.strip field == "manufacturer" = [((getOperator operator) Item_Manufacturer (Just $ T.strip value))]
                            | T.strip field == "model" = [((getOperator operator) Item_Model (Just $ T.strip value))]
                            | T.strip field == "categoryId" = [((getOperator operator) Item_CategoryId (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                            | otherwise = []

getInPredicate Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                              | T.strip field == "status" = [Item_Status <-. textToList value readEntityStatus]
                              | T.strip field == "itemType" = [Item_ItemType <-. textToList value readItemType]
                              | T.strip field == "name" = [Item_Name <-. textToList value P.id]
                              | T.strip field == "partNumber" = [Item_PartNumber <-. textToList value Just]
                              | T.strip field == "manufacturer" = [Item_Manufacturer <-. textToList value Just]
                              | T.strip field == "model" = [Item_Model <-. textToList value Just]
                              | T.strip field == "categoryId" = [Item_CategoryId <-. textToList value (\ e -> toSqlKey $ fromIntegral $ parseToInteger $ T.strip e)]
                              | otherwise = []

getPredicates [] = []
getPredicates (x:xs) | P.length p == 0 = getPredicates xs
                     | otherwise = p : getPredicates xs
                   where
                      p = (getPredicate x) P.++ (getInPredicate x)

conjunctionFilters xs = P.concat xs
unionFilters (x:xs) = foldl (||.) x xs

--itemsPageResolver :: PageArg -> Res e Handler (Page (Item Res))
itemsPageResolver PageArg {..} = lift $ do
--                        countItems <- runDB $ count ([Filter Item_Name (Left "%Michael%") (BackendSpecificFilter "like")] :: [Filter Item_])
                        let dbFilters = case filters of
                                            Nothing -> []
                                            Just f -> conjunctionFilters $ getPredicates f
--                        let dbFilters = case searchString of
--                                          Nothing -> conjunctionFilters filters
--                                          Just s -> ([Item_PartNumber ==. Just s] ||. [Item_Code ==. s] ||. [Item_Name `like`  s]) P.++ (conjunctionFilters filters)
                        countItems <- runDB $ count (dbFilters :: [Filter Item_])
                        items <- runDB $ selectList dbFilters [Asc Item_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                        let itemsQL = P.map (\r -> toItemQL r) items
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                          , hasPreview = pageIndex' * pageSize' > 0
                                                          , pageSize = pageSize'
                                                          , pageIndex = pageIndex'
                                    }
                        }
                         where
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
categoryResolver categoryId arg = lift $ do
                                      category <- dbFetchCategoryById categoryId
                                      return category

getUnitByIdResolver_ unitId _ = lift $ do
                                      unit <- dbFetchUnitById unitId
                                      return unit

-- toItemQL :: Entity Item_ -> (Item Res)
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
                                     , category = categoryResolver item_CategoryId
                                     , unit = getUnitByIdResolver_ item_UnitId
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
                                                                     , Item_CategoryId =. ((toSqlKey $ fromIntegral $ categoryId)::Category_Id)
                                                                     , Item_UnitId =. ((toSqlKey $ fromIntegral $ unitId)::Unit_Id)
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
                                        , item_CategoryId = ((toSqlKey $ fromIntegral $ categoryId)::Category_Id)
                                        , item_UnitId = ((toSqlKey $ fromIntegral $ unitId)::Unit_Id)
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
#endif
