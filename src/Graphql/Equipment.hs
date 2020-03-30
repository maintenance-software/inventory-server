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
                                 | T.strip field == "categoryId" = [getOperator operator (item ^. Item_CategoryId) (E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
                                 | otherwise = []

getEquipmentPredicate equipment Predicate {..} | T.strip field == "parentId" && T.strip operator == "=" && T.strip value == "null" = [E.isNothing (equipment ^. Equipment_ParentId)]
                                               | T.strip field == "parentId" && T.strip operator == "=" && T.strip value /= "" = [equipment ^. Equipment_ParentId E.==. E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value)]
                                               | otherwise = []

getInPredicate item Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
                                   | T.strip field == "name" = [(item ^. Item_Name) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "code" = [(item ^. Item_Code) `in_` (E.valList $ fromText P.id value)]
                                   | T.strip field == "status" = [(item ^. Item_Status) `in_` (E.valList $ fromText readEntityStatus value)]
                                   | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `in_` (E.valList $ fromText (\e -> Just e) value)]
                                   | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `in_` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                   | otherwise = []

getNotInPredicate item Predicate {..} | T.strip operator /= "not in" || T.strip value == "" = []
                                      | T.strip field == "name" = [(item ^. Item_Name) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "code" = [(item ^. Item_Code) `notIn` (E.valList $ fromText P.id value)]
                                      | T.strip field == "status" = [(item ^. Item_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
                                      | T.strip field == "partNumber" = [(item ^. Item_PartNumber) `notIn` (E.valList $ fromText (\e -> Just e) value)]
                                      | T.strip field == "categoryId" = [(item ^. Item_CategoryId) `notIn` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
                                      | otherwise = []
getPredicates equipment item [] = []
getPredicates equipment item (x:xs) | P.length p == 0 = getPredicates equipment item xs
                          | otherwise = p : getPredicates equipment item xs
                   where
                      p = (getEquipmentPredicate equipment x) P.++ (getPredicate item x) P.++ (getInPredicate item x) P.++ (getNotInPredicate item x)

conjunctionFilters (x:xs) = foldl (E.&&.) x xs
unionFilters (x:xs) = foldl (E.||.) x xs

--inventoryResolver :: () -> Res e Handler Inventories
equipmentResolver _ = pure Equipments { equipment = getEquipmentByIdResolver
                                      , page = equipmentsPageResolver
                                      , saveEquipment = saveEquipmentResolver
                                      }

--getInventoryByIdResolver :: GetEntityByIdArg -> Res e Handler (Inventory Res)
getEquipmentByIdResolver GetEntityByIdArg {..} = lift $ do
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


queryFilters equipment item PageArg {..} = do
                            let justFilters = case filters of Just a -> a; Nothing -> []
                            let predicates = P.concat $ getPredicates equipment item justFilters
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
                                        filters <- queryFilters equipment item page
                                        E.where_ filters
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

childrenQueryCount :: Item_Id -> PageArg -> Handler Int
childrenQueryCount parentId page =  do
                      res  <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment, item) -> do
                                        filters <- queryFilters equipment item page
                                        E.where_ (equipment ^. Equipment_ParentId E.==. E.val (Just parentId)
                                                 E.&&. equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                                 E.&&. filters
                                                 )
                                        return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

equipmentChildrenQuery :: Item_Id -> PageArg -> Handler [(Entity Equipment_, Entity Item_)]
equipmentChildrenQuery parentId page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment, item) -> do
                                     filters <- queryFilters equipment item page
                                     E.where_ (equipment ^. Equipment_ParentId E.==. E.val (Just parentId)
                                                E.&&. equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                                E.&&. filters
                                              )
                                     E.offset $ pageIndex_ * pageSize_
                                     E.limit pageSize_
                                     return (equipment, item)
                      return result
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

equipmentQuery :: PageArg -> Handler [(Entity Equipment_, Entity Item_)]
equipmentQuery page =  do
                      result <- runDB
                                   $ E.select
                                   $ E.from $ \(equipment `E.InnerJoin` item) -> do
                                        E.on $ equipment ^. Equipment_ItemId E.==. item ^. Item_Id
                                        filters <- queryFilters equipment item page
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

--createOrUpdateEquipment :: EquipmentArg -> Handler (Equipment MutRes)
createOrUpdateEquipment itemEntityId equipment = do
                let EquipmentArg {..} = equipment
                now <- liftIO getCurrentTime
                entityId <- if equipmentId > 0 then
                                do
                                  let itemId = (toSqlKey $ fromIntegral $ equipmentId) :: Item_Id
                                  let equipmentKey = Equipment_Key {unEquipment_Key  = itemId}
                                  _ <- runDB $ update equipmentKey [ Equipment_Priority =. priority
                                                                   , Equipment_HoursAverageDailyUse =. hoursAverageDailyUse
                                                                   , Equipment_OutOfService =. outOfService
                                                                   , Equipment_PurchaseDate =. case purchaseDate of Nothing -> Nothing; Just pd -> Just (read $ show pd :: UTCTime)
                                                                   , Equipment_ParentId =. case parentId of Nothing -> Nothing; Just p -> Just (toSqlKey $ fromIntegral $ p :: Item_Id)
                                                                   , Equipment_ModifiedDate =. Just now
                                                                   ]
                                  return equipmentKey
                               else do
                                  equipmentKey <- runDB $ insert $ fromEquipmentQL itemEntityId equipment now Nothing
                                  return equipmentKey
                return entityId

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

fromEquipmentQL :: Item_Id -> EquipmentArg -> UTCTime -> Maybe UTCTime -> Equipment_
fromEquipmentQL itemEntityId (EquipmentArg {..}) cd md = Equipment_ { equipment_ItemId = itemEntityId
                                                                    , equipment_Priority = priority
                                                                    , equipment_HoursAverageDailyUse = hoursAverageDailyUse
                                                                    , equipment_OutOfService = outOfService
                                                                    , equipment_PurchaseDate = case purchaseDate of Nothing -> Nothing; Just pd -> Just (read $ show pd :: UTCTime)
                                                                    , equipment_ParentId = case parentId of Nothing -> Nothing; Just p -> Just (toSqlKey $ fromIntegral $ p :: Item_Id)
                                                                    , equipment_CreatedDate = cd
                                                                    , equipment_ModifiedDate = md
                                                                    }

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
