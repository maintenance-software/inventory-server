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

module Graphql.Asset.Equipment.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Types (GQLType)
import Graphql.Utils (Page, PageArg, GetEntityByIdArg)
import Graphql.Asset.DataTypes
import Graphql.Asset.DataTypes
import Graphql.Asset.Category

data Equipment o = Equipment { equipmentId :: Int
                             , name :: Text
                             , description :: Maybe Text
                             , code :: Text
                             , partNumber :: Maybe Text
                             , manufacturer :: Maybe Text
                             , model :: Maybe Text
                             , notes:: Maybe Text
                             , status :: Text
                             , images :: [Text]
                             , priority :: Int
                             , hoursAverageDailyUse :: Int
                             , outOfService :: Bool
                             , purchaseDate :: Maybe Text
                             , children :: PageArg -> o () Handler (Page (Equipment o))
                             , parent :: Maybe(() -> o () Handler (Equipment o))
                             , category :: () -> o () Handler Category
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data Equipments o = Equipments { equipment :: GetEntityByIdArg -> o () Handler (Equipment o)
                               , page :: PageArg -> o () Handler (Page (Equipment o))
                               , saveEquipment :: EquipmentArg -> o () Handler (Equipment o)
                               } deriving (Generic, GQLType)

data EquipmentArg = EquipmentArg { equipmentId :: Int
                                 , name :: Text
                                 , description :: Maybe Text
                                 , code :: Text
                                 , partNumber :: Maybe Text
                                 , manufacturer :: Maybe Text
                                 , model :: Maybe Text
                                 , notes:: Maybe Text
                                 , status :: Text
                                 , images :: [Text]
                                 , priority :: Int
                                 , hoursAverageDailyUse :: Int
                                 , outOfService :: Bool
                                 , purchaseDate :: Maybe Text
                                 , parentId :: Maybe Int
                                 } deriving (Generic, GQLType)

{-
query {
  equipments  {
    page {
      totalCount
      content {
        equipmentId
        status
        name
        code
        priority
        outOfService
        children {
          equipmentId
          name
        }
      }
    }
  }
}

mutation {
  equipments  {
   saveEquipment(
    equipmentId : 121
   , name: "new item equipment"
   , description: "Maybe Text"
   , code: "Text_code"
   , status: "ACTIVE"
   , images: []
   , priority: 9
   , hoursAverageDailyUse: 7
   , outOfService: true
  ) {
    equipmentId
  }
  }
}
-}
