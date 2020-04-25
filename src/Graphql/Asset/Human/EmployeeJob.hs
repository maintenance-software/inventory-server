{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Graphql.Asset.Human.EmployeeJob (
      EmployeeJob
    , EmployeeJobArg
    , getEmployeeJobByIdResolver_
    , listEmployeeJobResolver
    , saveEmployeeJobResolver
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Data.Time

data EmployeeJob = EmployeeJob { employeeJobId :: Int
                               , tittle :: Text
                               , description :: Maybe Text
                               , createdDate :: Text
                               , modifiedDate :: Maybe Text
                               } deriving (Generic, GQLType)

data EmployeeJobArg = EmployeeJobArg { employeeJobId :: Int
                                     , tittle :: Text
                                     , description :: Maybe Text
                                     } deriving (Generic)

getEmployeeJobByIdResolver_ employeeJobId _ = lift $ do
                                      employeeJob <- runDB $ getJustEntity employeeJobId
                                      return $ toEmployeeJobQL employeeJob

--listEmployeeJobResolver :: () -> Res e Handler [EmployeeJob]
listEmployeeJobResolver _ = lift $ do
                               employeeJobs <- runDB $ selectList [] []
                               return $ P.map toEmployeeJobQL employeeJobs

--saveEmployeeJobResolver :: EmployeeJobArg -> MutRes e Handler EmployeeJob
saveEmployeeJobResolver arg = lift $ createOrUpdateEmployeeJob arg

createOrUpdateEmployeeJob :: EmployeeJobArg -> Handler EmployeeJob
createOrUpdateEmployeeJob employeeJobArg = do
                          let EmployeeJobArg {..} = employeeJobArg
                          now <- liftIO getCurrentTime
                          entityId <- if employeeJobId > 0 then
                                          do
                                            let employeeJobKey = (toSqlKey $ fromIntegral $ employeeJobId)::EmployeeJob_Id
                                            _ <- runDB $ update employeeJobKey [ EmployeeJob_Tittle =. tittle
                                                                                , EmployeeJob_Description =. description
                                                                                , EmployeeJob_ModifiedDate =. Just now
                                                                                ]
                                            return employeeJobKey
                                         else do
                                            employeeJobKey <- runDB $ insert $ fromEmployeeJobQL employeeJobArg now Nothing
                                            return employeeJobKey
                          employeeJob <- runDB $ getJustEntity entityId
                          return $ toEmployeeJobQL employeeJob

toEmployeeJobQL :: Entity EmployeeJob_ -> EmployeeJob
toEmployeeJobQL (Entity employeeJobId employeeJobArg) = EmployeeJob { employeeJobId = fromIntegral $ fromSqlKey employeeJobId
                                                                    , tittle = employeeJob_Tittle
                                                                    , description = employeeJob_Description
                                                                    , createdDate = fromString $ show employeeJob_CreatedDate
                                                                    , modifiedDate = m
                                                                    }
                          where
                            EmployeeJob_ {..} = employeeJobArg
                            m = case employeeJob_ModifiedDate of
                                  Just d -> Just $ fromString $ show d
                                  Nothing -> Nothing

fromEmployeeJobQL :: EmployeeJobArg -> UTCTime -> Maybe UTCTime -> EmployeeJob_
fromEmployeeJobQL (EmployeeJobArg {..}) cd md = EmployeeJob_ { employeeJob_Tittle = tittle
                                                             , employeeJob_Description = description
                                                             , employeeJob_CreatedDate = cd
                                                             , employeeJob_ModifiedDate = md
                                                             }
