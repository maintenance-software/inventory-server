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

module Graphql.Inventory (
      inventoryResolver
    , getInventoryByIdResolver_
    , saveInventoryResolver
    , toInventoryQL
--    , dbFetchInventoryById
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

--inventoryResolver :: () -> Res e Handler Inventories
inventoryResolver _ = pure Inventories { inventory = getInventoryByIdResolver
                                       , list = listInventoryResolver
                                       , saveInventory = saveInventoryResolver
                                       , saveInventoryItems = saveInventoryItemsResolver
                                       }

--getInventoryByIdResolver :: GetEntityByIdArg -> Res e Handler (Inventory Res)
getInventoryByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let inventoryId = (toSqlKey $ fromIntegral $ entityId)::Inventory_Id
                                              inventory <- runDB $ getJustEntity inventoryId
                                              return $ toInventoryQL inventory

getInventoryByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Inventory_Id -> () -> o () Handler (Inventory o)
getInventoryByIdResolver_ inventoryId _ = lift $ do
                                    inventory <- runDB $ getJustEntity inventoryId
                                    return $ toInventoryQL inventory

-- DB ACTIONS
--dbFetchInventoryById:: Inventory_Id -> Handler (Inventory Res)
--dbFetchInventoryById inventoryId = do
--                                      inventory <- runDB $ getJustEntity inventoryId
--                                      return $ toInventoryQL inventory

--dbFetchInventories:: Handler [Inventory Res]
--dbFetchInventories = do
--                       inventories <- runDB $ selectList ([] :: [Filter Inventory_]) []
--                       return $ P.map toInventoryQL inventories

--listInventoryResolver :: () -> Res e Handler [Inventory Res]
listInventoryResolver _ = lift $ do
                            inventories <- runDB $ selectList ([] :: [Filter Inventory_]) []
                            return $ P.map toInventoryQL inventories

--saveInventoryResolver :: InventoryArg -> MutRes e Handler (Inventory MutRes)
saveInventoryResolver arg = lift $ do
                                  inventoryId <- createOrUpdateInventory arg
                                  inventory <- runDB $ getJustEntity inventoryId
                                  return $ toInventoryQL inventory

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
--toInventoryQL :: Entity Inventory_ -> Inventory
toInventoryQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Inventory_ -> Inventory o
toInventoryQL (Entity inventoryId inventory) = Inventory { inventoryId = fromIntegral $ fromSqlKey inventoryId
                                                         , name = inventory_Name
                                                         , description = inventory_Description
                                                         , allowNegativeStocks = inventory_AllowNegativeStocks
                                                         , status = T.pack $ show inventory_Status
                                                         , inventoryItems = inventoryItemsPageResolver_ inventoryId
                                                         , createdDate = fromString $ show inventory_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            Inventory_ {..} = inventory
                                            m = case inventory_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

fromInventoryQL :: InventoryArg -> UTCTime -> Maybe UTCTime -> Inventory_
fromInventoryQL (InventoryArg {..}) cd md = Inventory_ { inventory_Name = name
                                                      , inventory_Description = description
                                                      , inventory_Status = readEntityStatus status
                                                      , inventory_AllowNegativeStocks = allowNegativeStocks
                                                      , inventory_CreatedDate = cd
                                                      , inventory_ModifiedDate = md
                                                      }

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
