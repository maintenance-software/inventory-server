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

data Pageable = Pageable { page :: Int, size :: Int } deriving (Generic)

instance GQLType Pageable where
    type  KIND Pageable = INPUT_OBJECT
    description = const $ Just $ pack "The item that holds the pageable information"

data ListArgs = ListArgs { queryString :: Text, pageable :: Maybe Pageable } deriving (Generic)

data GetEntityByIdArg = GetEntityByIdArg { entityId :: Int } deriving (Generic)

data EntityIdsArg = EntityIdsArg { entityIds :: [Int] } deriving (Generic)

data DummyArg = DummyArg {} deriving (Generic)

localDay :: IO Day
localDay = fmap utctDay getCurrentTime
