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

module Graphql.Asset.Equipment.Resolvers (
      equipmentResolver
    , toEquipmentQL
) where

import Import
import GHC.Generics
import Data.Morpheus.Types (lift)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Graphql.Asset.DataTypes
import Graphql.Category
import Graphql.Asset.Item.Persistence
import Graphql.Asset.Equipment.DataTypes
import Graphql.Asset.Equipment.Persistence

--inventoryResolver :: () -> Res e Handler Inventories
equipmentResolver _ = pure Equipments { equipment = getEquipmentByIdResolver
                                      , page = equipmentsPageResolver
                                      , saveEquipment = saveEquipmentResolver
                                      , setMaintenance = setMaintenanceResolver
                                      }

--getInventoryByIdResolver :: EntityIdArg -> Res e Handler (Inventory Res)
getEquipmentByIdResolver EntityIdArg {..} = lift $ do
                                              let itemId = (toSqlKey $ fromIntegral $ entityId) :: Item_Id
                                              let equipmentId = Equipment_Key {unEquipment_Key  = itemId}
                                              equipmentEntity <- runDB $ getJustEntity equipmentId
                                              itemEntity <- runDB $ getJustEntity itemId
                                              return $ toEquipmentQL equipmentEntity itemEntity

getEquipmentByIdResolver_ itemId _ = lift $ do
                                            let equipmentId = Equipment_Key {unEquipment_Key  = itemId}
                                            equipmentEntity <- runDB $ getJustEntity equipmentId
                                            itemEntity <- runDB $ getJustEntity itemId
                                            return $ toEquipmentQL equipmentEntity itemEntity

equipmentsPageResolver page = lift $ do
                        countItems <- equipmentQueryCount page
                        result <- equipmentQuery page
                        let itemsQL = P.map (\(e, i) -> toEquipmentQL e i) result
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex_ * pageSize_ + pageSize_ < countItems)
                                                          , hasPreview = pageIndex_ * pageSize_ > 0
                                                          , pageSize = pageSize_
                                                          , pageIndex = pageIndex_
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex_ = case pageIndex of Just  x  -> x; Nothing -> 0
                            pageSize_ = case pageSize of Just y -> y; Nothing -> 10

childrenResolver itemId page = lift $ do
                        countItems <- childrenQueryCount itemId page
                        result <- equipmentChildrenQuery itemId page
                        let itemsQL = P.map (\(e, i) -> toEquipmentQL e i) result
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex_ * pageSize_ + pageSize_ < countItems)
                                                          , hasPreview = pageIndex_ * pageSize_ > 0
                                                          , pageSize = pageSize_
                                                          , pageIndex = pageIndex_
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex_ = case pageIndex of Just  x  -> x; Nothing -> 0
                            pageSize_ = case pageSize of Just y -> y; Nothing -> 10

--saveEquipmentResolver :: EquipmentArg -> MutRes e Handler (Equipment MutRes)
saveEquipmentResolver arg = lift $ do
                                  itemId <- createOrUpdateItem itemArg
                                  equipmentId <- createOrUpdateEquipment itemId arg
                                  itemEntity <- runDB $ getJustEntity itemId
                                  equipmentEntity <- runDB $ getJustEntity equipmentId
                                  return $ toEquipmentQL equipmentEntity itemEntity
                      where
                        EquipmentArg {..} = arg
                        itemArg = ItemArg { itemId = equipmentId
                                          , code = code
                                          , name = name
                                          , defaultPrice = 0
                                          , description = description
                                          , partNumber = partNumber
                                          , manufacturer = manufacturer
                                          , model = model
                                          , itemType = "EQUIPMENT"
                                          , notes = notes
                                          , status = status
                                          , images = images
                                          , categoryId = Nothing
                                          , unitId = Nothing
                        }

setMaintenanceResolver arg = lift $ do
                      success <- setMaintenancePersistence arg
                      return success

--toEquipmentQL (Entity equipmentId equipment)  = Inventory { inventoryId = fromIntegral $ fromSqlKey inventoryId
toEquipmentQL equipmentEntity itemEntity = Equipment { equipmentId = fromIntegral $ fromSqlKey itemId
                                                     , name  = item_Name
                                                     , description  = item_Description
                                                     , code  = item_Code
                                                     , partNumber  = item_PartNumber
                                                     , manufacturer  = item_Manufacturer
                                                     , model  = item_Model
                                                     , notes = item_Notes
                                                     , status  = T.pack $ show item_Status
                                                     , images  = item_Images
                                                     , priority  = equipment_Priority
                                                     , hoursAverageDailyUse  = equipment_HoursAverageDailyUse
                                                     , outOfService  = equipment_OutOfService
                                                     , purchaseDate  = pd
                                                     , parent = case equipment_ParentId of Nothing -> Nothing; Just parentId -> Just $ getEquipmentByIdResolver_ parentId
                                                     , children = childrenResolver itemId
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
