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


data EntityStatus = ACTIVE | INACTIVE | EXPIRED | DELETED | UNKNOWN  deriving (Show, Read, Eq, Generic, GQLType)
derivePersistField "EntityStatus"

data ItemType = SPARE_PARTS | TOOLS | SUPPLIES | NONE deriving (Show, Read, Eq, Generic)
derivePersistField "ItemType"

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

readEntityStatus :: Text -> EntityStatus
readEntityStatus "ACTIVE" = ACTIVE
readEntityStatus "INACTIVE" = INACTIVE
readEntityStatus "EXPIRED" = EXPIRED
readEntityStatus "DELETED" = DELETED
readEntityStatus    _      = UNKNOWN

readItemType :: Text -> ItemType
readItemType "SPARE_PARTS" = SPARE_PARTS
readItemType "TOOLS" = TOOLS
readItemType "SUPPLIES" = SUPPLIES
readItemType _ = NONE
