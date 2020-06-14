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

module Graphql.Asset.Inventory.Persistence (createOrUpdateInventory) where

import Import
import GHC.Generics
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Data.Time
import Graphql.Asset.DataTypes

-- DB ACTIONS
--dbFetchInventoryById:: Inventory_Id -> Handler (Inventory Res)
--dbFetchInventoryById inventoryId = do
--                                      inventory <- runDB $ getJustEntity inventoryId
--                                      return $ toInventoryQL inventory

--dbFetchInventories:: Handler [Inventory Res]
--dbFetchInventories = do
--                       inventories <- runDB $ selectList ([] :: [Filter Inventory_]) []
--                       return $ P.map toInventoryQL inventories

--fetchInventoriesForItemQuery :: Item_Id -> Handler [Entity Inventory_]
--fetchInventoriesForItemQuery itemId = do
--                      result <- runDB
--                                   $ E.select
--                                   $ E.from $ \ inventory -> do
--                                        let subquery =
--                                              E.from $ \inventoryItem -> do
--                                              E.where_ (inventoryItem ^. InventoryItem_ItemId E.==. E.val itemId)
--                                              return (inventoryItem ^. InventoryItem_InventoryId)
--                                        E.where_ (inventory ^. Inventory_Id `E.in_` E.subList_select subquery)
--                                        E.orderBy [E.asc (inventory ^. Inventory_Id)]
--                                        return inventory
--                      return result

--createOrUpdateInventory :: InventoryArg -> Handler (Inventory MutRes)
createOrUpdateInventory inventory = do
                let InventoryArg {..} = inventory
                now <- liftIO getCurrentTime
                entityId <- if inventoryId > 0 then
                                do
                                  let inventoryKey = (toSqlKey $ fromIntegral $ inventoryId)::Inventory_Id
                                  _ <- runDB $ update inventoryKey [ Inventory_Name =. name
                                                                   , Inventory_Description =. description
                                                                   , Inventory_AllowNegativeStocks =. allowNegativeStocks
                                                                   , Inventory_Status =. readEntityStatus status
                                                                   , Inventory_ModifiedDate =. Just now
                                                                   ]
                                  return inventoryKey
                               else do
                                  inventoryKey <- runDB $ insert $ fromInventoryQL inventory now Nothing
                                  return inventoryKey
                return entityId

-- CONVERTERS
fromInventoryQL :: InventoryArg -> UTCTime -> Maybe UTCTime -> Inventory_
fromInventoryQL (InventoryArg {..}) cd md = Inventory_ { inventory_Name = name
                                                      , inventory_Description = description
                                                      , inventory_Status = readEntityStatus status
                                                      , inventory_AllowNegativeStocks = allowNegativeStocks
                                                      , inventory_CreatedDate = cd
                                                      , inventory_ModifiedDate = md
                                                      }
