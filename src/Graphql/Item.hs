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

module Graphql.Item (Items, Item, ItemArg, itemResolver, saveItemResolver, toItemQL) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Data.Time
import Graphql.Category

data Item = Item { itemId :: Int
                 , name :: Text
                 , unit :: Text
                 , defaultPrice :: Float
                 , description :: Text
                 , images :: [Text]
--                  , category :: DummyArg -> MutRes () Handler Category
                 , createdDate :: Text
                 , modifiedDate :: Maybe Text
                 } deriving (Generic, GQLType)

data Items m = Items { item :: GetEntityByIdArg -> m Item
                     , page :: PageArg -> m (Page Item)
                     } deriving (Generic, GQLType)

data ItemArg = ItemArg { itemId :: Int
                       , name :: Text
                       , unit :: Text
                       , defaultPrice :: Float
                       , description :: Text
--                        , code :: Text
                       , images :: [Text]
                       , categoryId :: Int
                       } deriving (Generic, GQLType)


-- Query Resolvers
findItemByIdResolver :: GetEntityByIdArg -> Res e Handler Item
findItemByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let itemId = (toSqlKey $ fromIntegral $ entityId)::Item_Id
                                              item <- runDB $ getJustEntity itemId
                                              return $ toItemQL item

listItemResolver :: PageArg -> Res e Handler (Page Item)
listItemResolver PageArg {..} = lift $ do
                        countItems <- runDB $ count ([] :: [Filter Item_])
                        items <- runDB $ selectList [] [Asc Item_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                        let itemsQL = P.map (\r -> toItemQL r) items
                        return Page { totalCount = countItems
                                    , content = itemsQL
                                    , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                          , hasPreview = pageIndex' * pageSize' > 0
                                                          , pageSize = pageSize'
                                                          , pageIndex = pageIndex'
                                    }
                        }
                         where
                          pageIndex' = case pageIndex of
                                        Just  x  -> x
                                        Nothing -> 0
                          pageSize' = case pageSize of
                                          Just y -> y
                                          Nothing -> 10

itemResolver :: Items (Res () Handler)
itemResolver = Items {  item = findItemByIdResolver, page = listItemResolver }

-- categoryResolver :: Category_Id -> DummyArg -> Res e Handler Category
categoryResolver categoryId arg = lift $ do
                                      category <- dbFetchCategoryById categoryId
                                      return category

toItemQL :: Entity Item_ -> Item
toItemQL (Entity itemId item) = Item { itemId = fromIntegral $ fromSqlKey itemId
                                     , name = item_Name
                                     , unit = item_Unit
                                     , defaultPrice = realToFrac item_DefaultPrice
                                     , description = item_Description
                                     , images = item_Images
--                                     , category = categoryResolver item_CategoryId
                                     , createdDate = fromString $ show item_CreatedDate
                                     , modifiedDate = m
                                     }
                            where
                              Item_ {..} = item
                              m = case item_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing
{-
data ItemMut = ItemMut { itemId :: Int
                       , name :: Text
                       , unit :: Text
                       , defaultPrice :: Float
                       , description :: Text
--                        , code :: Text
                       , images :: [Text]
                       , category :: DummyArg -> MutRes () Handler Category
                       , createdDate :: Text
                       , modifiedDate :: Maybe Text
                       } deriving (Generic, GQLType)
-}

-- Mutation Resolvers
saveItemResolver :: ItemArg -> MutRes e Handler Item
saveItemResolver arg = lift $ do
                              itemId <- createOrUpdateItem arg
                              item <- runDB $ getJustEntity itemId
                              return $ toItemQL item

createOrUpdateItem :: ItemArg -> Handler Item_Id
createOrUpdateItem item = do
                            let ItemArg {..} = item
                            now <- liftIO getCurrentTime
                            itemEntityId <- if itemId > 0 then
                                        do
                                         let itemKey = (toSqlKey $ fromIntegral $ itemId)::Item_Id
                                         _ <- runDB $ update itemKey [ Item_Name =. name
                                                                     , Item_Unit =. unit
                                                                     , Item_DefaultPrice =. realToFrac defaultPrice
                                                                     , Item_Description =. description
--                                                                      , Item_Code =. code
                                                                     , Item_Images =. images
                                                                     , Item_CategoryId =. ((toSqlKey $ fromIntegral $ categoryId)::Category_Id)
                                                                     , Item_ModifiedDate =. Just now
                                                                     ]
                                         return itemKey
                                      else do
                                            itemKey <- runDB $ insert $ fromItemQL item now Nothing
                                            return itemKey
                            return itemEntityId

fromItemQL :: ItemArg -> UTCTime -> Maybe UTCTime -> Item_
fromItemQL (ItemArg {..}) cd md = Item_ { item_Name = name
                                        , item_Unit = unit
                                        , item_DefaultPrice = realToFrac defaultPrice
                                        , item_Description = description
--                                         , item_Code = code
                                        , item_Images = images
                                        , item_CategoryId = (toSqlKey $ fromIntegral $ categoryId)::Category_Id
                                        , item_CreatedDate = cd
                                        , item_ModifiedDate = md
                                        }
{-
toItemMut :: Entity Item_ -> ItemMut
toItemMut (Entity itemId item) = ItemMut { itemId = fromIntegral $ fromSqlKey itemId
                                         , name = item_Name
                                         , unit = item_Unit
                                         , defaultPrice = realToFrac item_DefaultPrice
                                         , description = item_Description
--                                          , code = item_Code
                                         , images = item_Images
                                         , category = categoryResolver item_CategoryId
                                         , createdDate = fromString $ show item_CreatedDate
                                         , modifiedDate = md
                                         }
                            where
                              Item_ {..} = item
                              md = case item_ModifiedDate of
                                    Just d -> Just $ fromString $ show d
                                    Nothing -> Nothing
-}
{-
query {
  items {
    page(pageIndex:0, pageSize: 10) {
      totalCount
      pageInfo {
        pageIndex
        pageSize
        hasNext
        hasPreview
      }
      content {
        itemId
        name
        unit
        defaultPrice
        description
        code
        images
        createdDate
        modifiedDate
        category {
          categoryId
          name
        }
      }

    }
  }
}

mutation {
  saveRole(itemId:10, key: "test12", name: "sloss", description: "option" active: true) {
    itemId
    key
    description
    active
    createdDate
    modifiedDate
    privileges(entityIds: [16]) {
      privilegeId
      key
      description
      active
      createdDate
      modifiedDate
    }
  }
}

-}
