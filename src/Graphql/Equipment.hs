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

module Graphql.Equipment (
      equipmentResolver
--    , getInventoryByIdResolver_
--    , saveInventoryResolver
    , toEquipmentQL
    , Equipments
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Graphql.InventoryDataTypes
import Data.Time
import Graphql.InventoryItem
import Graphql.Item
import Graphql.Category

data Equipment o = Equipment { equipmentId :: Int
                             , name :: Text
                             , description :: Maybe Text
                             , partNumber :: Maybe Text
                             , manufacturer :: Maybe Text
                             , model :: Maybe Text
                             , itemType :: Text
                             , notes:: Maybe Text
                             , status :: Text
                             , images :: [Text]
                             , priority :: Int
                             , hoursAverageDailyUse :: Int
                             , outOfService :: Bool
                             , purchaseDate :: Maybe Text
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
                                 , partNumber :: Maybe Text
                                 , manufacturer :: Maybe Text
                                 , model :: Maybe Text
                                 , itemType :: Text
                                 , notes:: Maybe Text
                                 , status :: Text
                                 , images :: [Text]
                                 , priority :: Int
                                 , hoursAverageDailyUse :: Int
                                 , outOfService :: Bool
                                 , purchaseDate :: Maybe Text
                                 } deriving (Generic, GQLType)
--inventoryResolver :: () -> Res e Handler Inventories
equipmentResolver _ = pure Equipments { equipment = getEquipmentByIdResolver
--                                        page = pageEquipmentResolver
--                                      , saveEquipment = saveEquipmentResolver
                                      }

--getInventoryByIdResolver :: GetEntityByIdArg -> Res e Handler (Inventory Res)
getEquipmentByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let itemId = (toSqlKey $ fromIntegral $ entityId) :: Item_Id
                                              let equipmentId = Equipment_Key {unEquipment_Key  = itemId}
                                              equipmentEntity <- runDB $ getJustEntity equipmentId
                                              itemEntity <- runDB $ getJustEntity itemId
                                              return $ toEquipmentQL equipmentEntity itemEntity

testEq::Equipment_Id -> Bool
testEq _ = True
--getInventoryByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Inventory_Id -> () -> o () Handler (Inventory o)
--getInventoryByIdResolver_ inventoryId _ = lift $ do
--                                    inventory <- runDB $ getJustEntity inventoryId
--                                    return $ toInventoryQL inventory


--saveInventoryResolver :: InventoryArg -> MutRes e Handler (Inventory MutRes)
--saveInventoryResolver arg = lift $ do
--                                  inventoryId <- createOrUpdateInventory arg
--                                  inventory <- runDB $ getJustEntity inventoryId
--                                  return $ toInventoryQL inventory

--createOrUpdateInventory :: InventoryArg -> Handler (Inventory MutRes)
--createOrUpdateInventory inventory = do
--                let InventoryArg {..} = inventory
--                now <- liftIO getCurrentTime
--                entityId <- if inventoryId > 0 then
--                                do
--                                  let inventoryKey = (toSqlKey $ fromIntegral $ inventoryId)::Inventory_Id
--                                  _ <- runDB $ update inventoryKey [ Inventory_Name =. name
--                                                                   , Inventory_Description =. description
--                                                                   , Inventory_AllowNegativeStocks =. allowNegativeStocks
--                                                                   , Inventory_Status =. readEntityStatus status
--                                                                   , Inventory_ModifiedDate =. Just now
--                                                                   ]
--                                  return inventoryKey
--                               else do
--                                  inventoryKey <- runDB $ insert $ fromInventoryQL inventory now Nothing
--                                  return inventoryKey
--                return entityId

-- CONVERTERS
--toInventoryQL :: Entity Inventory_ -> Inventory
--toInventoryQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Inventory_ -> Inventory o
--toEquipmentQL (Entity equipmentId equipment)  = Inventory { inventoryId = fromIntegral $ fromSqlKey inventoryId
toEquipmentQL equipmentEntity itemEntity = Equipment { equipmentId = fromIntegral $ fromSqlKey itemId
                                                     , name  = item_Name
                                                     , description  = item_Description
                                                     , partNumber  = item_PartNumber
                                                     , manufacturer  = item_Manufacturer
                                                     , model  = item_Model
                                                     , itemType  = T.pack $ show item_ItemType
                                                     , notes = item_Notes
                                                     , status  = T.pack $ show item_Status
                                                     , images  = item_Images
                                                     , priority  = equipment_Priority
                                                     , hoursAverageDailyUse  = equipment_HoursAverageDailyUse
                                                     , outOfService  = equipment_OutOfService
                                                     , purchaseDate  = pd
                                                     , createdDate = fromString $ show equipment_CreatedDate
                                                     , modifiedDate = m
                                                     }
                                          where
                                            Entity _ equipment = equipmentEntity
                                            Entity itemId item = itemEntity
                                            Equipment_ {..} = equipment
                                            Item_ {..} = item
                                            m = case equipment_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing
                                            pd = case equipment_PurchaseDate of
                                                   Just d -> Just $ fromString $ show d
                                                   Nothing -> Nothing

--fromInventoryQL :: InventoryArg -> UTCTime -> Maybe UTCTime -> Inventory_
--fromInventoryQL (InventoryArg {..}) cd md = Inventory_ { inventory_Name = name
--                                                      , inventory_Description = description
--                                                      , inventory_Status = readEntityStatus status
--                                                      , inventory_AllowNegativeStocks = allowNegativeStocks
--                                                      , inventory_CreatedDate = cd
--                                                      , inventory_ModifiedDate = md
--                                                      }

{-
query {
  inventories(queryString: "") {
    inventoryId
    name
    description
  }
}

mutation {
  saveCategory(inventoryId: 0, name: "test", description: "sss") {
    inventoryId
    name
  }
}
-}
