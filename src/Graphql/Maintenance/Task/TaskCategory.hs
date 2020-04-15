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

module Graphql.Maintenance.Task.TaskCategory (
      TaskCategory
    , TaskCategoryArg
    , getTaskCategoryByIdResolver_
    , listTaskCategoryResolver
    , saveTaskCategoryResolver
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
--import Graphql.Utils
import Data.Time

data TaskCategory = TaskCategory { taskCategoryId :: Int
                                 , name :: Text
                                 , description :: Maybe Text
                                 , createdDate :: Text
                                 , modifiedDate :: Maybe Text
                                 } deriving (Generic, GQLType)

data TaskCategoryArg = TaskCategoryArg { taskCategoryId :: Int
                                       , name :: Text
                                       , description :: Maybe Text
                                       } deriving (Generic)

getTaskCategoryByIdResolver_ taskCategoryId arg = lift $ do
                                      taskCategory <- runDB $ getJustEntity taskCategoryId
                                      return $ toTaskCategoryQL taskCategory

listTaskCategoryResolver :: () -> Res e Handler [TaskCategory]
listTaskCategoryResolver _ = lift $ do
                               taskCategories <- runDB $ selectList [] []
                               return $ P.map toTaskCategoryQL taskCategories

saveTaskCategoryResolver :: TaskCategoryArg -> MutRes e Handler TaskCategory
saveTaskCategoryResolver arg = lift $ createOrUpdateTaskCategory arg

createOrUpdateTaskCategory :: TaskCategoryArg -> Handler TaskCategory
createOrUpdateTaskCategory taskCategoryArg = do
                          let TaskCategoryArg {..} = taskCategoryArg
                          now <- liftIO getCurrentTime
                          entityId <- if taskCategoryId > 0 then
                                          do
                                            let taskCategoryKey = (toSqlKey $ fromIntegral $ taskCategoryId)::TaskCategory_Id
                                            _ <- runDB $ update taskCategoryKey [ TaskCategory_Name =. name
                                                                                , TaskCategory_Description =. description
                                                                                , TaskCategory_ModifiedDate =. Just now
                                                                                ]
                                            return taskCategoryKey
                                         else do
                                            taskCategoryKey <- runDB $ insert $ fromTaskCategoryQL taskCategoryArg now Nothing
                                            return taskCategoryKey
                          taskCategory <- runDB $ getJustEntity entityId
                          return $ toTaskCategoryQL taskCategory

toTaskCategoryQL :: Entity TaskCategory_ -> TaskCategory
toTaskCategoryQL (Entity taskCategoryId taskCategoryArg) = TaskCategory { taskCategoryId = fromIntegral $ fromSqlKey taskCategoryId
                                                                        , name = taskCategory_Name
                                                                        , description = taskCategory_Description
                                                                        , createdDate = fromString $ show taskCategory_CreatedDate
                                                                        , modifiedDate = m
                                                                        }
                          where
                            TaskCategory_ {..} = taskCategoryArg
                            m = case taskCategory_ModifiedDate of
                                  Just d -> Just $ fromString $ show d
                                  Nothing -> Nothing

fromTaskCategoryQL :: TaskCategoryArg -> UTCTime -> Maybe UTCTime -> TaskCategory_
fromTaskCategoryQL (TaskCategoryArg {..}) cd md = TaskCategory_ { taskCategory_Name = name
                                                                , taskCategory_Description = description
                                                                , taskCategory_CreatedDate = cd
                                                                , taskCategory_ModifiedDate = md
                                                                }
