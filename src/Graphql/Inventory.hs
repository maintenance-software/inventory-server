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

module Graphql.Inventory (Inventory, InventoryArg, listInventoryResolver, saveInventoryResolver, toInventoryQL, dbFetchInventoryById) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Graphql.Utils
import Data.Time

data Inventory = Inventory { inventoryId :: Int
                           , name :: Text
                           , description :: Text
                           , createdDate :: Text
                           , modifiedDate :: Maybe Text
                           } deriving (Generic, GQLType)

data Inventories m = Inventories { inventory :: GetEntityByIdArg -> m Inventory
                                 , list :: () -> m [Inventory]
                                 } deriving (Generic, GQLType)

-- DB ACTIONS
dbFetchInventoryById:: Inventory_Id -> Handler Inventory
dbFetchInventoryById inventoryId = do
                                      inventory <- runDB $ getJustEntity inventoryId
                                      return $ toInventoryQL inventory

dbFetchInventories:: Handler [Inventory]
dbFetchInventories = do
                       inventories <- runDB $ selectList ([] :: [Filter Inventory_]) []
                       return $ P.map toInventoryQL inventories

listInventoryResolver :: () -> Res e Handler [Inventory]
listInventoryResolver _ = lift $ dbFetchInventories

-- Mutation
data InventoryArg = InventoryArg { inventoryId :: Int
                                 , name :: Text
                                 , description :: Text
                                 , active :: Bool
                                 } deriving (Generic)

saveInventoryResolver :: InventoryArg -> MutRes e Handler Inventory
saveInventoryResolver arg = lift $ createOrUpdateInventory arg

createOrUpdateInventory :: InventoryArg -> Handler Inventory
createOrUpdateInventory inventory = do
                let InventoryArg {..} = inventory
                now <- liftIO getCurrentTime
                entityId <- if inventoryId > 0 then
                                do
                                  let inventoryKey = (toSqlKey $ fromIntegral $ inventoryId)::Inventory_Id
                                  _ <- runDB $ update inventoryKey [ Inventory_Name =. name
                                                                   , Inventory_Description =. description
                                                                   , Inventory_ModifiedDate =. Just now
                                                                   ]
                                  return inventoryKey
                               else do
                                  inventoryKey <- runDB $ insert $ fromCategoryQL inventory now Nothing
                                  return inventoryKey
                response <- dbFetchInventoryById entityId
                return response

-- CONVERTERS
toInventoryQL :: Entity Inventory_ -> Inventory
toInventoryQL (Entity inventoryId inventory) = Inventory { inventoryId = fromIntegral $ fromSqlKey inventoryId
                                                         , name = inventory_Name
                                                         , description = inventory_Description
                                                         , createdDate = fromString $ show inventory_CreatedDate
                                                         , modifiedDate = m
                                                         }
                                          where
                                            Inventory_ {..} = inventory
                                            m = case inventory_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

fromCategoryQL :: InventoryArg -> UTCTime -> Maybe UTCTime -> Inventory_
fromCategoryQL (InventoryArg {..}) cd md = Inventory_ { inventory_Name = name
                                                      , inventory_Description = description
                                                      , inventory_Active = active
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
