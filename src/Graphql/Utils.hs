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

module Graphql.Utils where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..))
import Data.Time
import Enums

-- data Pageable = Pageable { pageIndex :: Int, pageSize :: Int } deriving (Generic)

data PageInfo = PageInfo { hasNext:: Bool
                         , hasPreview:: Bool
                         , pageSize :: Int
                         , pageIndex :: Int
                         } deriving (Generic, GQLType)

data Sort = Sort { isUnsorted :: Bool
                 , isSorted :: Bool
                 , direction :: Text
                 } deriving (Generic, GQLType)

data Page a = Page { totalCount :: Int
                   , content :: [a]
                   , pageInfo :: PageInfo
                   , sort :: Sort
                   } deriving (Generic, GQLType)

data EntityArg a = EntityArg { arg :: a } deriving (Generic, GQLType)

-- instance GQLType Pageable where
--     type  KIND Pageable = INPUT_OBJECT
--     description = const $ Just $ pack "The item that holds the pageable information"

data PageArg = PageArg { queryString :: Maybe Text
                       , pageIndex :: Maybe Int
                       , pageSize :: Maybe Int
                       } deriving (Generic)

data GetEntityByIdArg = GetEntityByIdArg { entityId :: Int } deriving (Generic)

data EntityIdsArg = EntityIdsArg { entityIds :: [Int] } deriving (Generic)

data DummyArg = DummyArg {} deriving (Generic)

localDay :: IO Day
localDay = fmap utctDay getCurrentTime
