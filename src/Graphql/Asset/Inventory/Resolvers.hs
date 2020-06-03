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

module Graphql.Asset.Inventory.Resolvers (
      inventoryResolver
    , getInventoryByIdResolver_
    , saveInventoryResolver
    , toInventoryQL
--    , dbFetchInventoryById
) where

import Import
import GHC.Generics
import Data.Morpheus.Types (lift)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Graphql.Asset.DataTypes
import Graphql.Asset.Inventory.Persistence
import Graphql.Asset.InventoryItem.Resolvers
import Graphql.Asset.Item.Resolvers
import Graphql.Asset.Inventory.Persistence
--inventoryResolver :: () -> Res e Handler Inventories
inventoryResolver :: (Applicative f, Typeable o,MonadTrans (o ())) => p -> f (Inventories o)
inventoryResolver _ = pure Inventories { inventory = getInventoryByIdResolver
                                       , list = listInventoryResolver
                                       , saveInventory = saveInventoryResolver
                                       , saveInventoryItems = saveInventoryItemsResolver
                                       , fetchInventoriesForItem= fetchInventoriesForItemResolver
                                       }

--getInventoryByIdResolver :: EntityIdArg -> Res e Handler (Inventory Res)
getInventoryByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler (Inventory o)
getInventoryByIdResolver EntityIdArg {..} = lift $ do
                                              let inventoryId = (toSqlKey $ fromIntegral $ entityId)::Inventory_Id
                                              inventory <- runDB $ getJustEntity inventoryId
                                              return $ toInventoryQL inventory

getInventoryByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Inventory_Id -> () -> o () Handler (Inventory o)
getInventoryByIdResolver_ inventoryId _ = lift $ do
                                    inventory <- runDB $ getJustEntity inventoryId
                                    return $ toInventoryQL inventory

--listInventoryResolver :: () -> Res e Handler [Inventory Res]
listInventoryResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => () -> o () Handler [Inventory o]
listInventoryResolver _ = lift $ do
                            inventories <- runDB $ selectList ([] :: [Filter Inventory_]) []
                            return $ P.map toInventoryQL inventories

fetchInventoriesForItemResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () Handler [Inventory o]
fetchInventoriesForItemResolver EntityIdArg{..} = lift $ do
                            let inventoryId = (toSqlKey $ fromIntegral $ entityId)::Item_Id
                            inventories <- fetchInventoriesForItemQuery inventoryId
                            return $ P.map toInventoryQL inventories

--saveInventoryResolver :: InventoryArg -> MutRes e Handler (Inventory MutRes)
saveInventoryResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => InventoryArg -> o () Handler (Inventory o)
saveInventoryResolver arg = lift $ do
                                  inventoryId <- createOrUpdateInventory arg
                                  inventory <- runDB $ getJustEntity inventoryId
                                  return $ toInventoryQL inventory

-- CONVERTERS
--toInventoryQL :: Entity Inventory_ -> Inventory
toInventoryQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Inventory_ -> Inventory o
toInventoryQL (Entity inventoryId inventory) = Inventory { inventoryId = fromIntegral $ fromSqlKey inventoryId
                                                         , name = inventory_Name
                                                         , description = inventory_Description
                                                         , allowNegativeStocks = inventory_AllowNegativeStocks
                                                         , status = T.pack $ show inventory_Status
                                                         , inventoryItems = inventoryItemsPageResolver_ inventoryId
                                                         , availableItems = availableItemsPageResolver_ inventoryId
                                                         , createdDate = fromString $ show inventory_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            Inventory_ {..} = inventory
                                            m = case inventory_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing
