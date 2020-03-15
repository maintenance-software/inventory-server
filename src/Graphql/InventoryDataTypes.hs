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

module Graphql.InventoryDataTypes (
    Inventory(..)
  , InventoryArg(..)
  , Inventories(..)
  , InventoryItems(..)
  , InventoryItem(..)
  , InventoryItemArg(..)
  , Item(..)
  , Items(..)
  , ItemArg(..)
  ) where

import GHC.Generics
import Import
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Graphql.Utils
import Graphql.Category
import Graphql.Unit
import Enums

data Inventory o = Inventory { inventoryId :: Int
                             , name :: Text
                             , description :: Text
                             , inventoryItems :: PageArg -> o () Handler (Page (InventoryItem o))
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data Inventories = Inventories { inventory :: GetEntityByIdArg ->  Res () Handler (Inventory Res)
                               , list :: () -> Res () Handler [Inventory Res]
                               } deriving (Generic, GQLType)

-- Mutation
data InventoryArg = InventoryArg { inventoryId :: Int
                                 , name :: Text
                                 , description :: Text
                                 , active :: Bool
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

data InventoryItems = InventoryItems { inventoryItem :: GetEntityByIdArg -> Res () Handler (InventoryItem Res)
                                     , page :: PageArg -> Res () Handler (Page (InventoryItem Res))
                                     } deriving (Generic, GQLType)

data InventoryItemArg = InventoryItemArg { inventoryItemId :: Int
                                         , level :: Int
                                         , maxLevelAllowed :: Int
                                         , minLevelAllowed :: Int
                                         , price :: Float
                                         , code :: Text
                                         , location :: Text
--                                         , status :: Text
                                         , inventoryId :: Int
                                         , dateExpiry :: Maybe Text
                                         , itemId :: Int
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
                   , category :: () -> o () Handler Category
                   , unit :: () -> o () Handler Unit
                   , inventoryItems :: PageArg -> o () Handler (Page (InventoryItem o))
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   } deriving (Generic, GQLType)

data Items o = Items { item :: GetEntityByIdArg -> o () Handler (Item o)
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
                       , categoryId :: Int
                       , unitId :: Int
                       } deriving (Generic, GQLType)
