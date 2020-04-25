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

module Graphql.Asset.InventoryItem.Persistence ( createOrUpdateInventoryItems, createOrUpdateInventoryItem) where

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
import Graphql.Asset.Unit
import Graphql.Asset.DataTypes
import Enums

createOrUpdateInventoryItems :: [InventoryItemArg] -> Handler [InventoryItem_Id]
createOrUpdateInventoryItems [] = pure []
createOrUpdateInventoryItems (x: xs) = do
                                        inventoryItemId <- createOrUpdateInventoryItem x
                                        inventoryItemIds <- createOrUpdateInventoryItems xs
                                        return (inventoryItemId : inventoryItemIds)

createOrUpdateInventoryItem :: InventoryItemArg -> Handler InventoryItem_Id
createOrUpdateInventoryItem inventoryItem = do
                            let InventoryItemArg {..} = inventoryItem
                            now <- liftIO getCurrentTime
                            itemEntityId <- if inventoryItemId > 0 then
                                        do
                                         let inventoryItemKey = (toSqlKey $ fromIntegral $ inventoryItemId)::InventoryItem_Id
--                                         let inventoryItemKey = InventoryItem_Key {unInventoryItem_Key  = itemId}
                                         _ <- runDB $ update inventoryItemKey [ InventoryItem_Level =. level
                                                                     , InventoryItem_MaxLevelAllowed =. maxLevelAllowed
                                                                     , InventoryItem_MinLevelAllowed =. minLevelAllowed
                                                                     , InventoryItem_Price =. realToFrac price
                                                                     , InventoryItem_Location =. location
--                                                                     , InventoryItem_Status =. readEntityStatus status
                                                                     , InventoryItem_DateExpiry =.  Just now
                                                                     , InventoryItem_InventoryId =. ((toSqlKey $ fromIntegral inventoryId)::Inventory_Id)
--                                                                     , InventoryItem_ItemId =. ((toSqlKey $ fromIntegral itemId)::Item_Id)
                                                                     , InventoryItem_ModifiedDate =. Just now
                                                                     ]
                                         return inventoryItemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromInventoryItemQL inventoryItem now Nothing
                                            return itemKey
                            return itemEntityId

fromInventoryItemQL :: InventoryItemArg -> UTCTime -> Maybe UTCTime -> InventoryItem_
fromInventoryItemQL (InventoryItemArg {..}) cd md = InventoryItem_ { inventoryItem_Level = level
                                                                   , inventoryItem_MaxLevelAllowed = maxLevelAllowed
                                                                   , inventoryItem_MinLevelAllowed = minLevelAllowed
                                                                   , inventoryItem_Price = realToFrac price
                                                                   , inventoryItem_Location = location
--                                                                   , inventoryItem_Status = readEntityStatus status
                                                                   , inventoryItem_DateExpiry = md
                                                                   , inventoryItem_ItemId = (toSqlKey $ fromIntegral itemId)::Item_Id
                                                                   , inventoryItem_InventoryId = ((toSqlKey $ fromIntegral $ inventoryId)::Inventory_Id)
                                                                   , inventoryItem_CreatedDate = cd
                                                                   , inventoryItem_ModifiedDate = md
                                                                   }
