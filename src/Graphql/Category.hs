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

module Graphql.Category (
    Category
    , CategoryArg
    , CategoryFilter
    , listCategoryResolver
    , saveCategoryResolver
    , toCategoryQL
    , getCategoryByIdResolver_
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Text as T
import Graphql.Utils
import Enums (readCategoryScope)
import Data.Time

data Category = Category { categoryId :: Int
                         , name :: Text
                         , scope :: Text
                         , description :: Text
                         , createdDate :: Text
                         , modifiedDate :: Maybe Text
                         } deriving (Generic, GQLType)

-- Mutation
data CategoryArg = CategoryArg { categoryId :: Int
                               , name :: Text
                               , scope :: Text
                               , description :: Text
                               } deriving (Generic)

data CategoryFilter = CategoryFilter { scope :: Text } deriving (Generic)
-- DB ACTIONS

getCategoryByIdResolver_ categoryId _ = lift $ do
                                      category <- runDB $ getJustEntity categoryId
                                      return $ toCategoryQL category

--dbFetchCategoryById:: Category_Id -> Handler Category
--dbFetchCategoryById categoryId = do
--                                      category <- runDB $ getJustEntity categoryId
--                                      return $ toCategoryQL category

--dbFetchCategories:: Handler [Category]
--dbFetchCategories = do
--                       categories <- runDB $ selectList [] []
--                       return $ P.map toCategoryQL categories

listCategoryResolver :: CategoryFilter -> Res e Handler [Category]
listCategoryResolver (CategoryFilter {..}) = lift $ do
                      categories <- runDB $ selectList [Category_Scope ==. (readCategoryScope scope)] []
                      return $ P.map toCategoryQL categories

saveCategoryResolver :: CategoryArg -> MutRes e Handler Category
saveCategoryResolver arg = lift $ do
                        categoryId <- createOrUpdateCategory arg
                        category <- runDB $ getJustEntity categoryId
                        return $ toCategoryQL category

createOrUpdateCategory :: CategoryArg -> Handler Category_Id
createOrUpdateCategory category = do
                let CategoryArg {..} = category
                now <- liftIO getCurrentTime
                entityId <- if categoryId > 0 then
                                do
                                  let categoryKey = (toSqlKey $ fromIntegral $ categoryId)::Category_Id
                                  _ <- runDB $ update categoryKey [ Category_Name =. name
                                                                  , Category_Description =. description
                                                                  , Category_Scope =. (readCategoryScope scope)
                                                                  , Category_ModifiedDate =. Just now
                                                                  ]
                                  return categoryKey
                               else do
                                  categoryKey <- runDB $ insert $ fromCategoryQL category now Nothing
                                  return categoryKey
                return entityId

-- CONVERTERS
toCategoryQL :: Entity Category_ -> Category
toCategoryQL (Entity categoryId category) = Category { categoryId = fromIntegral $ fromSqlKey categoryId
                                                     , name = category_Name
                                                     , description = category_Description
                                                     , scope = T.pack $ show category_Scope
                                                     , createdDate = fromString $ show category_CreatedDate
                                                     , modifiedDate = m
                                                     }
                                          where
                                            Category_ {..} = category
                                            m = case category_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing

fromCategoryQL :: CategoryArg -> UTCTime -> Maybe UTCTime -> Category_
fromCategoryQL (CategoryArg {..}) cd md = Category_ { category_Name = name
                                                 , category_Description = description
                                                 , category_Scope = (readCategoryScope scope)
                                                 , category_CreatedDate = cd
                                                 , category_ModifiedDate = md
                                                 }

{-
query {
  categories(queryString: "") {
    categoryId
    name
    description
  }
}

mutation {
  saveCategory(categoryId: 0, name: "test", description: "sss") {
    categoryId
    name
  }
}
-}
