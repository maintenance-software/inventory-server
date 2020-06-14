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

module Graphql.Asset.Item.Resolvers (
        itemResolver
      , getItemByIdResolver_
      , saveItemResolver
      , createOrUpdateItem
      , toItemQL
      , availableItemsPageResolver_
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Data.Text as T
import Prelude as P
import Graphql.Utils
import Enums
import Data.Time ()
import Graphql.Category
import Graphql.Asset.DataTypes
import Graphql.Asset.InventoryItem.Resolvers
import Graphql.Asset.Unit
import Graphql.Asset.Item.Persistence

-- Query Resolvers
getItemByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (Item o)
getItemByIdResolver EntityIdArg {..} = lift $ do
                                              let itemId = (toSqlKey $ fromIntegral $ entityId)::Item_Id
                                              item <- runDB $ getJustEntity itemId
                                              return $ toItemQL item

getItemByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Item_Id -> () -> o () Handler (Item o)
getItemByIdResolver_ itemId _ = lift $ do
                                         item <- runDB $ getJustEntity itemId
                                         return $ toItemQL item

--categoryResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Category_Id -> () -> o () Handler Category
--categoryResolver_ categoryId arg = lift $ do
--                                      category <- dbFetchCategoryById categoryId
--                                      return category

itemsPageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (Item o))
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

availableItemsPageResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => Inventory_Id -> PageArg -> t (HandlerFor App) (Page (Item o))
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

itemResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => p -> f (Items o)
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
                                     , category = case item_CategoryId of Nothing -> Nothing; Just c -> Just $ getCategoryByIdResolver_ c
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

changeItemStatusResolver :: MonadTrans t => EntityChangeStatusArg -> t Handler Bool
changeItemStatusResolver EntityChangeStatusArg {..} = lift $ do
                              () <- changeStatus entityIds (readEntityStatus status)
                              return True

saveItemResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => ItemArg -> t Handler (Item o)
saveItemResolver arg = lift $ do
                              itemId <- createOrUpdateItem arg
                              item <- runDB $ getJustEntity itemId
                              return $ toItemQL item
