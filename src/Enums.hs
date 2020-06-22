{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}

module Enums where

import Data.Text
import Database.Persist.TH
import Prelude
import GHC.Generics
-- import Data.Morpheus.Kind (ENUM)
import Data.Morpheus.Types (GQLType(..))


data EntityStatus = ACTIVE | INACTIVE | EXPIRED | DELETED | PENDING | UNKNOWN  deriving (Show, Read, Eq, Generic, GQLType)
derivePersistField "EntityStatus"

data CategoryScope = ITEM_CATEGORY | EMPLOYEE_JOB_CATEGORY | TASK_CATEGORY | SUBTASK_CATEGORY | EVENT_CATEGORY | NO_CATEGORY  deriving (Show, Read, Eq, Generic, GQLType)
derivePersistField "CategoryScope"

data ItemType = SPARE_PARTS | TOOLS | SUPPLIES | EQUIPMENT | NONE deriving (Show, Read, Eq, Generic)
derivePersistField "ItemType"

data TimeFrequency = DAY|WEEK|MONTH|YEAR deriving (Show, Read, Eq, Generic)
derivePersistField "TimeFrequency"

-- instance GQLType EntityStatus where
--   type KIND EntityStatus = ENUM

data Locale = EN_US | ES_US | ES_BO deriving (Eq, Generic)
derivePersistField "Locale"

instance Show Locale where
  show EN_US = "en_US"
  show ES_US = "es_US"
  show ES_BO = "es_BO"

instance Read Locale where
  readsPrec _ "en_US" = [(EN_US, "en_US")]
  readsPrec _ "es_US" = [(ES_US, "es_US")]
  readsPrec _ "es_BO" = [(ES_BO, "es_BO")]

-- instance GQLType Locale where
--   type KIND Locale = ENUM

readLocale :: Text -> Locale
readLocale "en_US" = EN_US
readLocale "es_US" = ES_US
readLocale "es_BO" = ES_BO
readLocale _ = EN_US

readEntityStatus :: Text -> EntityStatus
readEntityStatus "ACTIVE" = ACTIVE
readEntityStatus "INACTIVE" = INACTIVE
readEntityStatus "EXPIRED" = EXPIRED
readEntityStatus "DELETED" = DELETED
readEntityStatus "PENDING" = PENDING
readEntityStatus    _      = UNKNOWN

readItemType :: Text -> ItemType
readItemType "SPARE_PARTS" = SPARE_PARTS
readItemType "TOOLS" = TOOLS
readItemType "SUPPLIES" = SUPPLIES
readItemType "EQUIPMENT" = EQUIPMENT
readItemType _ = NONE

readTimeFrequency :: Text -> TimeFrequency
readTimeFrequency "DAY" = DAY
readTimeFrequency "WEEK" = WEEK
readTimeFrequency "MONTH" = MONTH
readTimeFrequency "YEAR" = YEAR
readTimeFrequency _ = DAY

readCategoryScope :: Text -> CategoryScope
readCategoryScope "ITEM_CATEGORY" = ITEM_CATEGORY
readCategoryScope "EMPLOYEE_JOB_CATEGORY" = EMPLOYEE_JOB_CATEGORY
readCategoryScope "TASK_CATEGORY" = TASK_CATEGORY
readCategoryScope "SUBTASK_CATEGORY" = SUBTASK_CATEGORY
readCategoryScope "EVENT_CATEGORY" = EVENT_CATEGORY
readCategoryScope _ = NO_CATEGORY
