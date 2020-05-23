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

module Graphql.Asset.Human.Resolvers (
      employeeResolver
    , toEmployeeQL
    , getEmployeeByIdResolver_
) where

import Import
import GHC.Generics
import Data.Morpheus.Types (lift)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Maybe (maybeToList, listToMaybe)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils
import Graphql.Asset.Human.DataTypes
import Graphql.Asset.Human.Persistence
import Graphql.Category
import Graphql.Admin.DataTypes (PersonArg(..))
import Graphql.Admin.Person (createOrUpdatePerson, addressResolver, contactInfoResolver)

--inventoryResolver :: () -> Res e Handler Inventories
employeeResolver _ = pure Employees { employee = getEmployeeByIdResolver
                                    , page = employeesPageResolver
                                    , saveEmployee = saveEmployeeResolver
                                    }

--getInventoryByIdResolver :: GetEntityByIdArg -> Res e Handler (Inventory Res)
getEmployeeByIdResolver GetEntityByIdArg {..} = lift $ do
                                              let personId = (toSqlKey $ fromIntegral $ entityId) :: Person_Id
                                              let employeeId = Employee_Key {unEmployee_Key  = personId}
                                              employeeEntity <- runDB $ getJustEntity employeeId
                                              personEntity <- runDB $ getJustEntity personId
                                              return $ toEmployeeQL personEntity employeeEntity

getEmployeeByIdResolver_ personId _ = lift $ do
                                            let employeeId = Employee_Key {unEmployee_Key  = personId}
                                            employeeEntity <- runDB $ getJustEntity employeeId
                                            personEntity <- runDB $ getJustEntity personId
                                            return $ toEmployeeQL personEntity employeeEntity

employeesPageResolver page = lift $ do
                        countItems <- employeeQueryCount page
                        result <- employeeQuery page
                        let employeesQL = P.map (\(p, e) -> toEmployeeQL p e) result
                        return Page { totalCount = countItems
                                    , content = employeesQL
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

--saveEmployeeResolver :: EquipmentArg -> MutRes e Handler (Employee MutRes)
saveEmployeeResolver arg = lift $ do
                                  let EmployeeArg {..} = arg
                                  let personArg = PersonArg { personId = employeeId
                                                            , firstName = firstName
                                                            , lastName = lastName
                                                            , documentType = documentType
                                                            , documentId = documentId
                                                            , address = address
                                                            , contactInfo = contactInfo
                                                            }
                                  personId <- createOrUpdatePerson personArg
                                  employeeId <- createOrUpdateEmployee personId arg
                                  let employeeKey = Employee_Key {unEmployee_Key  = employeeId}
                                  personEntity <- runDB $ getJustEntity personId
                                  employeeEntity <- runDB $ getJustEntity employeeKey
                                  return $ toEmployeeQL personEntity employeeEntity


--toEmployeeQL (Entity employeeId employee)  = Inventory { inventoryId = fromIntegral $ fromSqlKey inventoryId
toEmployeeQL personEntity employeeEntity = Employee { employeeId = fromIntegral $ fromSqlKey personId
                                                    , firstName = person_FirstName
                                                    , lastName = person_LastName
                                                    , documentType = person_DocumentType
                                                    , documentId = person_DocumentId
                                                    , hireDate = (case employee_HireDate of Nothing -> Nothing; Just hd -> Just $ fromString $ show hd)
                                                    , salary = realToFrac employee_Salary
                                                    , employeeCategory = getCategoryByIdResolver_ employee_EmployeeCategoryId
                                                    , address = addressResolver personId
                                                    , contactInfo = contactInfoResolver personId
                                                    , createdDate = fromString $ show employee_CreatedDate
                                                    , modifiedDate = m
                                                    }
                                          where
                                            Entity _ employee = employeeEntity
                                            Entity personId person = personEntity
                                            Employee_ {..} = employee
                                            Person_ {..} = person
                                            m = case employee_ModifiedDate of
                                                  Just d -> Just $ fromString $ show d
                                                  Nothing -> Nothing
