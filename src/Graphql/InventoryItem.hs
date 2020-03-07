{-# LANGUAGE CPP               #-}
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

#ifdef include_InventoryItem

--toInventoryItemQL :: Entity InventoryItem_ -> InventoryItem
toInventoryItemQL (Entity inventoryItemId inventoryItem) = InventoryItem { inventoryItemId = fromIntegral $ fromSqlKey inventoryItemId
                                                                         , level = inventoryItem_Level
                                                                         , maxLevelAllowed = inventoryItem_MaxLevelAllowed
                                                                         , minLevelAllowed = inventoryItem_MinLevelAllowed
                                                                         , price = realToFrac inventoryItem_Price
                                                                         , code = inventoryItem_Code
                                                                         , location = inventoryItem_Location
--                                                                         , status = T.pack $ show inventoryItem_Status
                                                                         , inventory =  inventoryResolver_ inventoryItem_InventoryId
                                                                         , dateExpiry = de
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

#undef include_InventoryItem

#elif 1

module Graphql.InventoryItem (
        inventoryItemResolver
      , saveInventoryItemResolver
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
import Graphql.Category
import Graphql.Inventory (toInventoryQL)
import Graphql.InventoryDataTypes
import Enums

-- Query Resolvers
findInventoryItemByIdResolver :: GetEntityByIdArg -> Res e Handler (InventoryItem Res)
findInventoryItemByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let inventoryItemId = (toSqlKey $ fromIntegral $ entityId)::InventoryItem_Id
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

listInventoryItemResolver :: PageArg -> Res e Handler (Page (InventoryItem Res))
listInventoryItemResolver PageArg {..} = lift $ do
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

inventoryItemResolver :: () -> Res e Handler InventoryItems
inventoryItemResolver _ = pure InventoryItems { inventoryItem = findInventoryItemByIdResolver, page = listInventoryItemResolver }

--toInventoryItemQL :: Entity InventoryItem_ -> InventoryItem
toInventoryItemQL (Entity inventoryItemId inventoryItem) = InventoryItem { inventoryItemId = fromIntegral $ fromSqlKey inventoryItemId
                                                                         , level = inventoryItem_Level
                                                                         , maxLevelAllowed = inventoryItem_MaxLevelAllowed
                                                                         , minLevelAllowed = inventoryItem_MinLevelAllowed
                                                                         , price = realToFrac inventoryItem_Price
                                                                         , code = inventoryItem_Code
                                                                         , location = inventoryItem_Location
--                                                                         , status = T.pack $ show inventoryItem_Status
                                                                         , dateExpiry = de
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

-- Mutation Resolvers
saveInventoryItemResolver :: InventoryItemArg -> MutRes e Handler (InventoryItem MutRes)
saveInventoryItemResolver arg = lift $ do
                              inventoryItemId <- createOrUpdateInventoryItem arg
                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                              return $ toInventoryItemQL inventoryItem

createOrUpdateInventoryItem :: InventoryItemArg -> Handler InventoryItem_Id
createOrUpdateInventoryItem inventoryItem = do
                            let InventoryItemArg {..} = inventoryItem
                            now <- liftIO getCurrentTime
                            itemEntityId <- if inventoryItemId > 0 then
                                        do
                                         let itemKey = (toSqlKey $ fromIntegral $ inventoryItemId)::InventoryItem_Id
                                         _ <- runDB $ update itemKey [ InventoryItem_Level =. level
                                                                     , InventoryItem_MaxLevelAllowed =. maxLevelAllowed
                                                                     , InventoryItem_MinLevelAllowed =. minLevelAllowed
                                                                     , InventoryItem_Price =. realToFrac price
                                                                     , InventoryItem_Code =. code
                                                                     , InventoryItem_Location =. location
--                                                                     , InventoryItem_Status =. readEntityStatus status
                                                                     , InventoryItem_DateExpiry =.  Just now
                                                                     , InventoryItem_InventoryId =. ((toSqlKey $ fromIntegral inventoryId)::Inventory_Id)
                                                                     , InventoryItem_ItemId =. ((toSqlKey $ fromIntegral itemId)::Item_Id)
                                                                     , InventoryItem_ModifiedDate =. Just now
                                                                     ]
                                         return itemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromInventoryItemQL inventoryItem now Nothing
                                            return itemKey
                            return itemEntityId

fromInventoryItemQL :: InventoryItemArg -> UTCTime -> Maybe UTCTime -> InventoryItem_
fromInventoryItemQL (InventoryItemArg {..}) cd md = InventoryItem_ { inventoryItem_Level = level
                                                                   , inventoryItem_MaxLevelAllowed = maxLevelAllowed
                                                                   , inventoryItem_MinLevelAllowed = minLevelAllowed
                                                                   , inventoryItem_Price = realToFrac price
                                                                   , inventoryItem_Code = code
                                                                   , inventoryItem_Location = location
--                                                                   , inventoryItem_Status = readEntityStatus status
                                                                   , inventoryItem_DateExpiry = md
                                                                   , inventoryItem_ItemId = (toSqlKey $ fromIntegral itemId)::Item_Id
                                                                   , inventoryItem_InventoryId = ((toSqlKey $ fromIntegral $ inventoryId)::Inventory_Id)
                                                                   , inventoryItem_CreatedDate = cd
                                                                   , inventoryItem_ModifiedDate = md
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
        inventoryItemId
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
  saveRole(inventoryItemId:10, key: "test12", name: "sloss", description: "option" active: true) {
    inventoryItemId
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
