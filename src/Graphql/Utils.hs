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

import System.Random
import Prelude as P
import qualified Data.Text as T
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
                       , filters :: Maybe [Predicate]
                       } deriving (Generic)

data GetEntityByIdArg = GetEntityByIdArg { entityId :: Int } deriving (Generic)

data EntityIdsArg = EntityIdsArg { entityIds :: [Int] } deriving (Generic)

data Predicate = Predicate { field :: Text, operator :: Text, value :: Text} deriving (Generic)

instance GQLType Predicate where
    type  KIND Predicate = INPUT_OBJECT
    description = const $ Just $ pack "This field holds predicate information"

--data DummyArg = DummyArg {} deriving (Generic)

localDay :: IO Day
localDay = fmap utctDay getCurrentTime


genRandomAlphaNumString :: Int -> IO String
genRandomAlphaNumString 0 = return []
genRandomAlphaNumString n = do
                             let s = ['0'..'9'] P.++ ['A'..'Z']
                             r <- randomRIO (0, (P.length s) - 1)
                             let c = s !! r
                             s <- genRandomAlphaNumString (n - 1)
                             return (c:s)

parseToInteger :: Text -> Int
parseToInteger str = read $ T.unpack str :: Int
