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

module Graphql.InventoryItem (InventoryItems, InventoryItem, InventoryItemArg, inventoryItemResolver, saveInventoryItemResolver, toInventoryItemQL) where

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
import Enums

data InventoryItem = InventoryItem { inventoryItemId :: Int
                                   , level :: Int
                                   , price :: Float
                                   , code :: Text
                                   , status :: Text
                                   , dateExpiry :: Text
                                   , createdDate :: Text
                                   , modifiedDate :: Maybe Text
                                   } deriving (Generic, GQLType)

data InventoryItems m = InventoryItems { inventoryItem :: GetEntityByIdArg -> m InventoryItem
                                       , page :: PageArg -> m (Page InventoryItem)
                                       } deriving (Generic, GQLType)

data InventoryItemArg = InventoryItemArg { inventoryItemId :: Int
                                         , level :: Int
                                         , price :: Float
                                         , code :: Text
                                         , status :: Text
                                         , dateExpiry :: Text
                                         } deriving (Generic, GQLType)

-- Query Resolvers
findInventoryItemByIdResolver :: GetEntityByIdArg -> Res e Handler InventoryItem
findInventoryItemByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let inventoryItemId = (toSqlKey $ fromIntegral $ entityId)::InventoryItem_Id
                                              inventoryItem <- runDB $ getJustEntity inventoryItemId
                                              return $ toInventoryItemQL inventoryItem

listInventoryItemResolver :: PageArg -> Res e Handler (Page InventoryItem)
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

inventoryItemResolver :: InventoryItems (Res () Handler)
inventoryItemResolver = InventoryItems {  inventoryItem = findInventoryItemByIdResolver, page = listInventoryItemResolver }

-- categoryResolver :: Category_Id -> DummyArg -> Res e Handler Category
categoryResolver categoryId arg = lift $ do
                                      category <- dbFetchCategoryById categoryId
                                      return category

toInventoryItemQL :: Entity InventoryItem_ -> InventoryItem
toInventoryItemQL (Entity inventoryItemId inventoryItem) = InventoryItem { inventoryItemId = fromIntegral $ fromSqlKey inventoryItemId
                                                                         , level = inventoryItem_Level
                                                                         , price = realToFrac inventoryItem_Price
                                                                         , code = inventoryItem_Code
                                                                         , status = T.pack $ show inventoryItem_Status
                                                                         , dateExpiry = fromString $ show inventoryItem_CreatedDate
                                                                         , createdDate = fromString $ show inventoryItem_CreatedDate
                                                                         , modifiedDate = m
                                                                         }
                            where
                              InventoryItem_ {..} = inventoryItem
                              m = case inventoryItem_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing

-- Mutation Resolvers
saveInventoryItemResolver :: InventoryItemArg -> MutRes e Handler InventoryItem
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
                                                                     , InventoryItem_Price =. realToFrac price
                                                                     , InventoryItem_Code =. code
                                                                     , InventoryItem_Status =. readEntityStatus status
                                                                     , InventoryItem_DateExpiry =.  now
                                                                     , InventoryItem_ModifiedDate =. Just now
                                                                     ]
                                         return itemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromInventoryItemQL inventoryItem now Nothing
                                            return itemKey
                            return itemEntityId

fromInventoryItemQL :: InventoryItemArg -> UTCTime -> Maybe UTCTime -> InventoryItem_
fromInventoryItemQL (InventoryItemArg {..}) cd md = InventoryItem_ { inventoryItem_Level = level
                                                                   , inventoryItem_Price = realToFrac price
                                                                   , inventoryItem_Code = code
                                                                   , inventoryItem_Status = readEntityStatus status
                                                                   , inventoryItem_DateExpiry = cd
                                                                   , inventoryItem_ItemId = (toSqlKey $ fromIntegral 0)::Item_Id
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
