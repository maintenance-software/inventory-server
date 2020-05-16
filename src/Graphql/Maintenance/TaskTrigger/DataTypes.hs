{-# LANGUAGE CPP                   #-}
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

module Graphql.Maintenance.TaskTrigger.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Enums
import Data.Time
import Graphql.Category
import Graphql.Asset.Unit

data TaskTrigger o = TaskTrigger { taskTriggerId :: Int
                                 , kind :: Text
                                 , description :: Text
                                 , fixedSchedule :: Maybe Bool
                                 , frequency :: Maybe Int
                                 , readType :: Maybe Text
                                 , limit :: Maybe Text
                                 , repeat :: Maybe Bool
                                 , operator :: Maybe Text
                                 , value :: Maybe Text
                                 , timeFrequency :: Maybe Text
                                 , unit :: Maybe(() -> o () Handler Unit)
                                 , eventTriggerCategory :: Maybe(() -> o () Handler Category)
                                 , createdDate :: Text
                                 , modifiedDate :: Maybe Text
                                 } deriving (Generic, GQLType)

data TaskTriggerArg = TaskTriggerArg { taskTriggerId :: Int
                                     , kind :: Text
                                     , description :: Text
                                     , fixedSchedule :: Maybe Bool
                                     , frequency :: Maybe Int
                                     , readType :: Maybe Text
                                     , limit :: Maybe Text
                                     , repeat :: Maybe Bool
                                     , operator :: Maybe Text
                                     , value :: Maybe Text
                                     , timeFrequency :: Maybe Text
                                     , unitId :: Maybe Int
                                     , eventTriggerCategoryId :: Maybe Int
                                     } deriving (Generic)

instance GQLType TaskTriggerArg where
    type  KIND TaskTriggerArg = INPUT_OBJECT
    description = const $ Just $ pack "This field holds TaskTriggerArg Input information"
