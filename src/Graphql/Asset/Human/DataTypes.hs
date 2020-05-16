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

module Graphql.Asset.Human.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Types (GQLType)
import Graphql.Utils (Page, PageArg, GetEntityByIdArg)
import Graphql.Person (Address, AddressArg, ContactInfo, ContactInfoArg)
import Graphql.Category

data Employee o = Employee { employeeId :: Int
                           , firstName :: Text
                           , lastName :: Text
                           , documentType :: Text
                           , documentId :: Text
                           , hireDate :: Maybe Text
                           , salary :: Float
                           , employeeCategory :: () -> o () Handler Category
                           , address :: () -> o () Handler (Maybe Address)
                           , contactInfo :: () -> o () Handler [ContactInfo]
                           , createdDate :: Text
                           , modifiedDate :: Maybe Text
                           } deriving (Generic, GQLType)

data Employees o = Employees { employee :: GetEntityByIdArg -> o () Handler (Employee o)
                             , page :: PageArg -> o () Handler (Page (Employee o))
                             , saveEmployee :: EmployeeArg -> o () Handler (Employee o)
                             } deriving (Generic, GQLType)

data EmployeeArg = EmployeeArg { employeeId :: Int
                               , firstName :: Text
                               , lastName :: Text
                               , documentType :: Text
                               , documentId :: Text
                               , hireDate :: Maybe Text
                               , salary :: Float
                               , employeeCategoryId :: Int
                               , address :: Maybe AddressArg
                               , contactInfo :: [ContactInfoArg]
                               } deriving (Generic, GQLType)

{-
query {
  employees  {
    page {
      totalCount
      content {
        employeeId
        status
        name
        code
        priority
        outOfService
        children {
          employeeId
          name
        }
      }
    }
  }
}

mutation {
  employees  {
   saveEmployee(
    employeeId : 121
   , name: "new item employee"
   , description: "Maybe Text"
   , code: "Text_code"
   , status: "ACTIVE"
   , images: []
   , priority: 9
   , hoursAverageDailyUse: 7
   , outOfService: true
  ) {
    employeeId
  }
  }
}
-}
