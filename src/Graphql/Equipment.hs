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
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Data.Maybe (maybeToList, listToMaybe)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils hiding (getOperator, conjunctionFilters, unionFilters)
import Graphql.InventoryDataTypes
import Data.Time
import Graphql.InventoryItem
import Graphql.Item
import Graphql.Category

data Equipment o = Equipment { equipmentId :: Int
                             , name :: Text
                             , description :: Maybe Text
                             , code :: Text
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
                                 , code :: Text
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

getOperator "=" = (E.==.)
getOperator ">" = (E.>.)
getOperator ">=" = (E.>=.)
getOperator "<=" = (E.<=.)
getOperator "<" = (E.<.)

getPredicate item Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
                                 | T.strip field == "name" = [getOperator operator (item ^. Item_Name) (E.val value)]
                                 | T.strip field == "code" = [getOperator operator (item ^. Item_Code) (E.val $ T.strip value)]
                                 | T.strip field == "status" = [getOperator operator (item ^. Item_Status) (E.val (readEntityStatus $ T.strip value))]
                                 | T.strip field == "partNumber" = [getOperator operator (item ^. Item_PartNumber) (E.val $ Just $ T.strip value)]
                                 | T.strip field == "categoryId" = [getOperator operator (item ^. Item_CategoryId) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                 | otherwise = []

getInPredicate item Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                   | T.strip field == "name" = [(item ^. Item_Name) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "code" = [(item ^. Item_Code) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "status" = [(item ^. Item_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                   | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `in_` (E.valList $ fromText (\e -> Just e) value)]
                                   | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `in_` (E.valList $ fromText (\ e -> toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                   | otherwise = []

getNotInPredicate item Predicate {..} | T.strip operator /= "not in" || T.strip value == "" = []
                                      | T.strip field == "name" = [(item ^. Item_Name) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "code" = [(item ^. Item_Code) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "status" = [(item ^. Item_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                      | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `notIn` (E.valList $ fromText (\e -> Just e) value)]
                                      | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `notIn` (E.valList $ fromText (\ e -> toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                      | otherwise = []
getPredicates item [] = []
getPredicates item (x:xs) | P.length p == 0 = getPredicates item xs
                          | otherwise = p : getPredicates item xs
                   where
                      p = (getPredicate item x) P.++ (getInPredicate item x) P.++ (getNotInPredicate item x)

conjunctionFilters (x:xs) = foldl (E.&&.) x xs
unionFilters (x:xs) = foldl (E.||.) x xs

--inventoryResolver :: () -> Res e Handler Inventories
equipmentResolver _ = pure Equipments { equipment = getEquipmentByIdResolver
                                      , page = equipmentsPageResolver
--                                      , saveEquipment = saveEquipmentResolver
                                      }

--getInventoryByIdResolver :: GetEntityByIdArg -> Res e Handler (Inventory Res)
getEquipmentByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let itemId = (toSqlKey $ fromIntegral $ entityId) :: Item_Id
                                              let equipmentId = Equipment_Key {unEquipment_Key  = itemId}
                                              equipmentEntity <- runDB $ getJustEntity equipmentId
                                              itemEntity <- runDB $ getJustEntity itemId
                                              return $ toEquipmentQL equipmentEntity itemEntity

queryFilters item PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getPredicates item justFilters
                            let predicates_ = if P.length predicates > 0 then
                                                  conjunctionFilters predicates
                                              else
                                                  (item ^. Item_Id E.==. item ^. Item_Id)
                            let searchFilters = case searchString of
                                                  Just s -> [item ^. Item_Code E.==. E.val s, item ^. Item_Name `E.like` (%) ++. E.val s ++. (%)]
                                                  Nothing -> [item ^. Item_Id E.==. item ^. Item_Id]
                            let searchFilters_ = unionFilters searchFilters
                            return (searchFilters_ E.&&. predicates_)

equipmentQueryCount :: PageArg -> Handler Int
equipmentQueryCount page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        filters <- queryFilters item page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

equipmentQuery :: PageArg -> Handler [(Entity Equipment_, Entity Item_)]
equipmentQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        filters <- queryFilters item page
                                        E.where_ filters
                                        E.offset $ pageIndex_ * pageSize_
                                        E.limit pageSize_
                                        return (equipment, item)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

equipmentsPageResolver page = lift $ do
                        countItems <- equipmentQueryCount page
                        result <- equipmentQuery page
                        let itemsQL = P.map (\(e, i) -> toEquipmentQL e i) result
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                          , hasPreview = pageIndex' * pageSize' > 0
                                                          , pageSize = pageSize'
                                                          , pageIndex = pageIndex'
                                    }
                        }
                         where
                            PageArg {..} = page
                            pageIndex' = case pageIndex of
                                          Just  x  -> x
                                          Nothing -> 0
                            pageSize' = case pageSize of
                                            Just y -> y
                                            Nothing -> 10
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
                                                     , code  = item_Code
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
