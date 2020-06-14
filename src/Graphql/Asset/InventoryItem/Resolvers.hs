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

module Graphql.Asset.InventoryItem.Resolvers (
        inventoryItemsResolver
      , getInventoryItemByIdResolver_
      , inventoryItemsPageResolver_
      , inventoryItemsItemPageResolver_
      , saveInventoryItemResolver
      , saveInventoryItemsResolver
      , toInventoryItemQL
) where

import Import
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Graphql.Utils
import {-# SOURCE #-} Graphql.Asset.Item.Resolvers
import {-# SOURCE #-} Graphql.Asset.Inventory.Resolvers
import Graphql.Asset.Unit ()
import Graphql.Asset.DataTypes
import Graphql.Asset.DataTypes ()
import Graphql.Asset.InventoryItem.Persistence

-- Query Resolvers
inventoryItemsResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (InventoryItems o)
inventoryItemsResolver _ = pure InventoryItems { inventoryItem = findInventoryItemByIdResolver
                                               , page = inventoryItemsPageResolver
                                               , saveInventoryItem = saveInventoryItemResolver
                                               }

findInventoryItemByIdResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => EntityIdArg -> t Handler (InventoryItem o)
findInventoryItemByIdResolver EntityIdArg {..} = lift $ do
                                              let inventoryItemId = (toSqlKey $ fromIntegral $ entityId)::InventoryItem_Id
--                                              let inventoryItemId = InventoryItem_Key {unInventoryItem_Key  = itemId}
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

getInventoryItemByIdResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => InventoryItem_Id -> () -> t Handler (InventoryItem o)
getInventoryItemByIdResolver_ inventoryItemId _ = lift $ do
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

inventoryItemsPageResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => Inventory_Id -> PageArg -> t Handler (Page (InventoryItem o))
inventoryItemsPageResolver_ inventoryId (PageArg {..}) = lift $ do
                                    countItems <- runDB $ count ([InventoryItem_InventoryId ==. inventoryId] :: [Filter InventoryItem_])
                                    items <- runDB $ selectList [InventoryItem_InventoryId ==. inventoryId] [Asc InventoryItem_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                                    let itemsQL = P.map (\r -> toInventoryItemQL r) items
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

inventoryItemsItemPageResolver_ :: (Typeable o, MonadTrans t, MonadTrans (o ())) => Item_Id -> PageArg -> t Handler (Page (InventoryItem o))
inventoryItemsItemPageResolver_ itemId (PageArg {..}) = lift $ do
                                    countItems <- runDB $ count ([InventoryItem_ItemId ==. itemId] :: [Filter InventoryItem_])
                                    items <- runDB $ selectList [InventoryItem_ItemId ==. itemId] [Asc InventoryItem_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                                    let itemsQL = P.map (\r -> toInventoryItemQL r) items
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

inventoryItemsPageResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (InventoryItem o))
inventoryItemsPageResolver PageArg {..} = lift $ do
                        countItems <- runDB $ count ([] :: [Filter InventoryItem_])
                        items <- runDB $ selectList [] [Asc InventoryItem_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                        let itemsQL = P.map (\r -> toInventoryItemQL r) items
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

-- Mutation Resolvers
--saveInventoryItemResolver :: InventoryItemArg -> MutRes e Handler (InventoryItem MutRes)
saveInventoryItemResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => InventoryItemArg -> t Handler (InventoryItem o)
saveInventoryItemResolver arg = lift $ do
                              inventoryItemId <- createOrUpdateInventoryItem arg
                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                              return $ toInventoryItemQL inventoryItem

saveInventoryItemsResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => InventoryItemsArg -> t Handler [InventoryItem o]
saveInventoryItemsResolver arg = lift $ do
                              let InventoryItemsArg {..} = arg
                              let inventoryItemArgs =  P.map (\itemId -> InventoryItemArg { inventoryItemId = 0
                                                                                          , level = level
                                                                                          , maxLevelAllowed = maxLevelAllowed
                                                                                          , minLevelAllowed = minLevelAllowed
                                                                                          , price = price
                                                                                          , location = location
                                                                                          , inventoryId = inventoryId
                                                                                          , dateExpiry = dateExpiry
                                                                                          , itemId = itemId
                                                                                          }) itemIds
                              inventoryItemIds <- createOrUpdateInventoryItems inventoryItemArgs
                              inventoryItems <- runDB $ mapM getJustEntity inventoryItemIds
                              return $ P.map toInventoryItemQL inventoryItems

toInventoryItemQL :: (Typeable o, MonadTrans (o ())) => Entity InventoryItem_ -> InventoryItem o
toInventoryItemQL (Entity inventoryItemId inventoryItem) = InventoryItem { inventoryItemId = fromIntegral $ fromSqlKey inventoryItemId
                                                                         , level = inventoryItem_Level
                                                                         , maxLevelAllowed = inventoryItem_MaxLevelAllowed
                                                                         , minLevelAllowed = inventoryItem_MinLevelAllowed
                                                                         , price = realToFrac inventoryItem_Price
                                                                         , location = inventoryItem_Location
--                                                                         , status = T.pack $ show inventoryItem_Status
                                                                         , dateExpiry = de
                                                                         , inventory = getInventoryByIdResolver_ inventoryItem_InventoryId
                                                                         , item = getItemByIdResolver_ inventoryItem_ItemId
                                                                         , createdDate = fromString $ show inventoryItem_CreatedDate
                                                                         , modifiedDate = m
                                                                         }
                            where
                              InventoryItem_ {..} = inventoryItem
                              m = case inventoryItem_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing
                              de = case inventoryItem_DateExpiry of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing
