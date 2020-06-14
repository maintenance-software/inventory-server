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

module Graphql.Asset.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Types (GQLType)
import Graphql.Utils (Page, PageArg, EntityIdArg, EntityChangeStatusArg)
import Graphql.Category
import Graphql.Asset.Unit

data Inventory o = Inventory { inventoryId :: Int
                             , name :: Text
                             , description :: Text
                             , allowNegativeStocks :: Bool
                             , status :: Text
                             , inventoryItems :: PageArg -> o () Handler (Page (InventoryItem o))
                             , availableItems :: PageArg -> o () Handler (Page (Item o))
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data Inventories o = Inventories { inventory :: EntityIdArg ->  o () Handler (Inventory o)
                                 , list :: () -> o () Handler [Inventory o]
                                 , saveInventory :: InventoryArg -> o () Handler (Inventory o)
                                 , saveInventoryItems :: InventoryItemsArg -> o () Handler [InventoryItem o]
                                 } deriving (Generic, GQLType)

-- Mutation
data InventoryArg = InventoryArg { inventoryId :: Int
                                 , name :: Text
                                 , description :: Text
                                 , allowNegativeStocks :: Bool
                                 , status :: Text
                                 } deriving (Generic)

data InventoryItem o = InventoryItem { inventoryItemId :: Int
                                   , level :: Int
                                   , maxLevelAllowed :: Int
                                   , minLevelAllowed :: Int
                                   , price :: Float
                                   , location :: Text
                                   , dateExpiry :: Maybe Text
                                   , inventory :: () -> o () Handler (Inventory o)
                                   , item :: () -> o () Handler (Item o)
                                   , createdDate :: Text
                                   , modifiedDate :: Maybe Text
                                   } deriving (Generic, GQLType)

data InventoryItems o = InventoryItems { inventoryItem :: EntityIdArg -> o () Handler (InventoryItem o)
                                       , page :: PageArg -> o () Handler (Page (InventoryItem o))
                                       , saveInventoryItem :: InventoryItemArg -> o () Handler (InventoryItem o)
                                       } deriving (Generic, GQLType)

data InventoryItemArg = InventoryItemArg { inventoryItemId :: Int
                                         , level :: Int
                                         , maxLevelAllowed :: Int
                                         , minLevelAllowed :: Int
                                         , price :: Float
                                         , location :: Text
                                         , inventoryId :: Int
                                         , dateExpiry :: Maybe Text
                                         , itemId :: Int
                                         } deriving (Generic, GQLType)

data InventoryItemsArg = InventoryItemsArg { level :: Int
                                           , maxLevelAllowed :: Int
                                           , minLevelAllowed :: Int
                                           , price :: Float
                                           , location :: Text
                                           , dateExpiry :: Maybe Text
                                           , inventoryId :: Int
                                           , itemIds :: [Int]
                                           } deriving (Generic, GQLType)

data Item o = Item { itemId :: Int
                   , code :: Text
                   , name :: Text
                   , defaultPrice :: Float
                   , description :: Maybe Text
                   , partNumber :: Maybe Text
                   , manufacturer :: Maybe Text
                   , model :: Maybe Text
                   , itemType :: Text
                   , notes:: Maybe Text
                   , status :: Text
                   , images :: [Text]
                   , category :: Maybe(() -> o () Handler Category)
                   , unit :: Maybe (() -> o () Handler Unit)
                   , inventoryItems :: PageArg -> o () Handler (Page (InventoryItem o))
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   } deriving (Generic, GQLType)

data Items o = Items { item :: EntityIdArg -> o () Handler (Item o)
                     , page :: PageArg -> o () Handler (Page (Item o))
                     , saveItem :: ItemArg -> o () Handler (Item o)
                     , changeItemStatus :: EntityChangeStatusArg -> o () Handler Bool
                     } deriving (Generic, GQLType)

data ItemArg = ItemArg { itemId :: Int
                       , code :: Text
                       , name :: Text
                       , defaultPrice :: Float
                       , description :: Maybe Text
                       , partNumber :: Maybe Text
                       , manufacturer :: Maybe Text
                       , model :: Maybe Text
                       , itemType :: Text
                       , notes :: Maybe Text
                       , status :: Text
                       , images :: [Text]
                       , categoryId :: Maybe Int
                       , unitId :: Maybe Int
                       } deriving (Generic, GQLType)


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
