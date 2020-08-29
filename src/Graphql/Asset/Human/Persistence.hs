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

module Graphql.Asset.Human.Persistence (
      employeeQuery
    , createOrUpdateEmployee
    , employeeQueryCount
) where

import Import
import GHC.Generics
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Database.Esqueleto      as E
import Database.Esqueleto      ((^.), (?.), (%), (++.), notIn, in_)
import Data.Maybe (maybeToList, listToMaybe)
import Prelude as P
import qualified Data.Text as T
import Enums
import Graphql.Utils (Page, PageArg(..), unionFilters)
import Graphql.Asset.Human.DataTypes
import Data.Time



--getPersonPredicate person Predicate {..} | T.strip field == "" || (T.strip operator) `P.elem` ["", "in", "like"] || T.strip value == "" = []
--                                         | T.strip field == "name" = [getOperator operator (person ^. Employee_Name) (E.val value)]
--                                         | T.strip field == "code" = [getOperator operator (person ^. Employee_Code) (E.val $ T.strip value)]
--                                         | T.strip field == "status" = [getOperator operator (person ^. Employee_Status) (E.val (readEntityStatus $ T.strip value))]
--                                         | T.strip field == "partNumber" = [getOperator operator (person ^. Employee_PartNumber) (E.val $ Just $ T.strip value)]
--                                         | T.strip field == "categoryId" = [getOperator operator (person ^. Employee_CategoryId) (E.val (Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
--                                         | T.strip field == "employeeId" = [getOperator operator (person ^. Employee_Id) (E.val (toSqlKey $ fromIntegral $ parseToInteger $ T.strip value))]
--                                         | otherwise = []

--getPersonInPredicate person Predicate {..} | T.strip operator /= "in" || T.strip value == "" = []
--                                           | T.strip field == "name" = [(person ^. Employee_Name) `in_` (E.valList $ fromText P.id value)]
--                                           | T.strip field == "code" = [(person ^. Employee_Code) `in_` (E.valList $ fromText P.id value)]
--                                           | T.strip field == "status" = [(person ^. Employee_Status) `in_` (E.valList $ fromText readEntityStatus value)]
--                                           | T.strip field == "partNumber" = [(person ^. Employee_PartNumber) `in_` (E.valList $ fromText (\e -> Just e) value)]
--                                           | T.strip field == "categoryId" = [(person ^. Employee_CategoryId) `in_` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
--                                           | otherwise = []
--
--getPersonNotInPredicate person Predicate {..} | T.strip operator /= "notIn" || T.strip value == "" = []
--                                              | T.strip field == "name" = [(person ^. Employee_Name) `notIn` (E.valList $ fromText P.id value)]
--                                              | T.strip field == "code" = [(person ^. Employee_Code) `notIn` (E.valList $ fromText P.id value)]
--                                              | T.strip field == "status" = [(person ^. Employee_Status) `notIn` (E.valList $ fromText readEntityStatus value)]
--                                              | T.strip field == "partNumber" = [(person ^. Employee_PartNumber) `notIn` (E.valList $ fromText (\e -> Just e) value)]
--                                              | T.strip field == "categoryId" = [(person ^. Employee_CategoryId) `notIn` (E.valList $ fromText (\ e -> Just $ toSqlKey $ fromIntegral $ parseToInteger $ T.strip e) value)]
--                                              | otherwise = []

--getPersonPredicates person [] = []
--getPersonPredicates person (x:xs) | P.length p == 0 = getPersonPredicates employee person xs
--                                    | otherwise = p : getPersonPredicates employee person xs
--                   where
--                      p = (getPersonPredicate person x) P.++ (getPersonInPredicate person x) P.++ (getPersonNotInPredicate person x)

searchFilters person PageArg {..} = do
                            let f = case searchString of
                                     Just s -> [ person ^. Person_DocumentId E.==. E.val s
                                               , person ^. Person_FirstName `E.like` (%) ++. E.val s ++. (%)
                                               , person ^. Person_LastName `E.like` (%) ++. E.val s ++. (%)
                                               ]
                                     Nothing -> [person ^. Person_Id E.==. person ^. Person_Id]
                            return $ unionFilters f

employeeQueryCount :: PageArg -> Handler Int
employeeQueryCount page =  do
                      res  <- runDB
                               $ E.select
                               $ E.from $ \(person `E.InnerJoin` employee) -> do
                                    E.on $ person ^. Person_Id E.==. employee ^. Employee_EmployeeId
                                    filters <- searchFilters person page
                                    E.where_ filters
                                    return E.countRows
                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ res

employeeQuery :: PageArg -> Handler [(Entity Person_, Entity Employee_)]
employeeQuery page =  do
                      res  <- runDB
                                $ E.select
                                $ E.from $ \(person `E.InnerJoin` employee) -> do
                                     E.on $ person ^. Person_Id E.==. employee ^. Employee_EmployeeId
                                     filters <- searchFilters person page
                                     E.where_ filters
                                     E.offset $ pageIndex_ * pageSize_
                                     E.limit pageSize_
                                     return (person, employee)
                      return res
                      where
                        PageArg {..} = page
                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

createOrUpdateEmployee :: Person_Id -> EmployeeArg -> Handler Person_Id
createOrUpdateEmployee personId employee = do
                let EmployeeArg {..} = employee
                now <- liftIO getCurrentTime
--                let personId = (toSqlKey $ fromIntegral $ employeeId) :: Person_Id
                let employeeKey = Employee_Key {unEmployee_Key  = personId}
                maybeEmployee <- runDB $ get employeeKey
                _ <- if not $ isNothing maybeEmployee then
                         do
                           _ <- runDB $ update employeeKey [ Employee_HireDate =. case hireDate of Nothing -> Nothing; Just hd -> Just (read $ T.unpack hd :: UTCTime)
                                                           , Employee_Salary =. realToFrac salary
                                                           , Employee_EmployeeCategoryId =. (toSqlKey $ fromIntegral $ employeeCategoryId :: Category_Id)
                                                           , Employee_ModifiedDate =. Just now
                                                           ]
                           return ()
                        else do
                           _ <- runDB $ insert $ fromEmployeeQL personId employee now Nothing
                           return ()
                return personId

fromEmployeeQL :: Person_Id -> EmployeeArg -> UTCTime -> Maybe UTCTime -> Employee_
fromEmployeeQL personId (EmployeeArg {..}) cd md = Employee_ { employee_EmployeeId = personId
                                                             , employee_HireDate = case hireDate of Nothing -> Nothing; Just hd -> Just (read $ T.unpack hd :: UTCTime)
                                                             , employee_Salary = realToFrac salary
                                                             , employee_EmployeeCategoryId = (toSqlKey $ fromIntegral $ employeeCategoryId :: Category_Id)
                                                             , employee_CreatedDate = cd
                                                             , employee_ModifiedDate = md
                                                             }
