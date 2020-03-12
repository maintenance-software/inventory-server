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
    , listCategoryResolver
    , saveCategoryResolver
    , toCategoryQL
    , dbFetchCategoryById
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Graphql.Utils
import Data.Time

data Category = Category { categoryId :: Int
                         , name :: Text
                         , description :: Text
                         , createdDate :: Text
                         , modifiedDate :: Maybe Text
                         } deriving (Generic, GQLType)

-- DB ACTIONS
dbFetchCategoryById:: Category_Id -> Handler Category
dbFetchCategoryById categoryId = do
                                      category <- runDB $ getJustEntity categoryId
                                      return $ toCategoryQL category

dbFetchCategories:: Handler [Category]
dbFetchCategories = do
                       categories <- runDB $ selectList [] []
                       return $ P.map toCategoryQL categories

listCategoryResolver :: () -> Res e Handler [Category]
listCategoryResolver _ = lift $ dbFetchCategories

-- Mutation
data CategoryArg = CategoryArg { categoryId :: Int
                               , name :: Text
                               , description :: Text
                               } deriving (Generic)

saveCategoryResolver :: CategoryArg -> MutRes e Handler Category
saveCategoryResolver arg = lift $ createOrUpdateCategory arg

createOrUpdateCategory :: CategoryArg -> Handler Category
createOrUpdateCategory category = do
                let CategoryArg {..} = category
                now <- liftIO getCurrentTime
                entityId <- if categoryId > 0 then
                                do
                                  let categoryKey = (toSqlKey $ fromIntegral $ categoryId)::Category_Id
                                  _ <- runDB $ update categoryKey [ Category_Name =. name
                                                                  , Category_Description =. description
                                                                  , Category_ModifiedDate =. Just now
                                                                  ]
                                  return categoryKey
                               else do
                                  categoryKey <- runDB $ insert $ fromCategoryQL category now Nothing
                                  return categoryKey
                response <- dbFetchCategoryById entityId
                return response

-- CONVERTERS
toCategoryQL :: Entity Category_ -> Category
toCategoryQL (Entity categoryId category) = Category { categoryId = fromIntegral $ fromSqlKey categoryId
                                                     , name = category_Name
                                                     , description = category_Description
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
