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
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Data.Text as T
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Data.Time
import {-# SOURCE #-} Graphql.Asset.Item.Resolvers
import {-# SOURCE #-} Graphql.Asset.Inventory.Resolvers
import Graphql.Asset.Unit
import Graphql.Asset.DataTypes
import Enums
import Graphql.Asset.DataTypes
import Graphql.Asset.InventoryItem.Persistence

-- Query Resolvers
--findInventoryItemByIdResolver :: GetEntityByIdArg -> Res e Handler (InventoryItem Res)
findInventoryItemByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let inventoryItemId = (toSqlKey $ fromIntegral $ entityId)::InventoryItem_Id
--                                              let inventoryItemId = InventoryItem_Key {unInventoryItem_Key  = itemId}
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

--getInventoryItemByIdResolver_ :: InventoryItem_Id -> () -> Res e Handler (InventoryItem Res)
getInventoryItemByIdResolver_ inventoryItemId _ = lift $ do
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

--inventoryItemsResolver :: Inventory_Id -> PageArg -> Res e Handler (Page InventoryItem)
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

--inventoryItemsResolver :: Inventory_Id -> PageArg -> Res e Handler (Page InventoryItem)
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

--inventoryItemsPageResolver :: PageArg -> Res e Handler (Page (InventoryItem Res))
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

--inventoryItemsResolver :: () -> Res e Handler InventoryItems
inventoryItemsResolver _ = pure InventoryItems { inventoryItem = findInventoryItemByIdResolver
                                               , page = inventoryItemsPageResolver
                                               , saveInventoryItem = saveInventoryItemResolver
                                               }

-- Mutation Resolvers
--saveInventoryItemResolver :: InventoryItemArg -> MutRes e Handler (InventoryItem MutRes)
saveInventoryItemResolver arg = lift $ do
                              inventoryItemId <- createOrUpdateInventoryItem arg
                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                              return $ toInventoryItemQL inventoryItem

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

--toInventoryItemQL :: Entity InventoryItem_ -> InventoryItem
toInventoryItemQL (Entity inventoryItemId inventoryItem) = InventoryItem { inventoryItemId = fromIntegral $ fromSqlKey inventoryItem_ItemId
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
