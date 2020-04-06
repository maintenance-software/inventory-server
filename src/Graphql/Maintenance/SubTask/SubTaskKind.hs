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

module Graphql.Maintenance.SubTask.SubTaskKind (
      SubTaskKind
    , SubTaskKindArg
    , getSubTaskKindByIdResolver_
    , listSubTaskKindResolver
    , saveSubTaskKindResolver
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Data.Time

data SubTaskKind = SubTaskKind { subTaskKindId :: Int
                               , name :: Text
                               , description :: Maybe Text
                               , createdDate :: Text
                               , modifiedDate :: Maybe Text
                               } deriving (Generic, GQLType)

data SubTaskKindArg = SubTaskKindArg { subTaskKindId :: Int
                                     , name :: Text
                                     , description :: Maybe Text
                                     } deriving (Generic)

getSubTaskKindByIdResolver_ subTaskKindId arg = lift $ do
                                      subTaskKindArg <- runDB $ getJustEntity subTaskKindId
                                      return $ toSubTaskKindQL subTaskKindArg

listSubTaskKindResolver :: () -> Res e Handler [SubTaskKind]
listSubTaskKindResolver _ = lift $ do
                               subTaskKinds <- runDB $ selectList [] []
                               return $ P.map toSubTaskKindQL subTaskKinds

saveSubTaskKindResolver :: SubTaskKindArg -> MutRes e Handler SubTaskKind
saveSubTaskKindResolver arg = lift $ createOrUpdateSubTaskKind arg

createOrUpdateSubTaskKind :: SubTaskKindArg -> Handler SubTaskKind
createOrUpdateSubTaskKind subTaskKindArg = do
                          let SubTaskKindArg {..} = subTaskKindArg
                          now <- liftIO getCurrentTime
                          entityId <- if subTaskKindId > 0 then
                                          do
                                            let subTaskKindKey = (toSqlKey $ fromIntegral $ subTaskKindId)::SubTaskKind_Id
                                            _ <- runDB $ update subTaskKindKey [ SubTaskKind_Name =. name
                                                                               , SubTaskKind_Description =. description
                                                                               , SubTaskKind_ModifiedDate =. Just now
                                                                               ]
                                            return subTaskKindKey
                                         else do
                                            subTaskKindKey <- runDB $ insert $ fromTaskCategoryQL subTaskKindArg now Nothing
                                            return subTaskKindKey
                          subTaskKind <- runDB $ getJustEntity entityId
                          return $ toSubTaskKindQL subTaskKind

toSubTaskKindQL :: Entity SubTaskKind_ -> SubTaskKind
toSubTaskKindQL (Entity subTaskKindId subTaskKindArg) = SubTaskKind { subTaskKindId = fromIntegral $ fromSqlKey subTaskKindId
                                                                    , name = subTaskKind_Name
                                                                    , description = subTaskKind_Description
                                                                    , createdDate = fromString $ show subTaskKind_CreatedDate
                                                                    , modifiedDate = m
                                                                    }
                          where
                            SubTaskKind_ {..} = subTaskKindArg
                            m = case subTaskKind_ModifiedDate of
                                  Just d -> Just $ fromString $ show d
                                  Nothing -> Nothing

fromTaskCategoryQL :: SubTaskKindArg -> UTCTime -> Maybe UTCTime -> SubTaskKind_
fromTaskCategoryQL (SubTaskKindArg {..}) cd md = SubTaskKind_ { subTaskKind_Name = name
                                                              , subTaskKind_Description = description
                                                              , subTaskKind_CreatedDate = cd
                                                              , subTaskKind_ModifiedDate = md
                                                              }
