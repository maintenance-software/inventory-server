{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}

module Enums where

import Database.Persist.TH
import Prelude
import           GHC.Generics
import Data.Morpheus.Kind (ENUM)
import Data.Morpheus.Types (GQLType(..))


data EntityStatus = ACTIVE | INACTIVE | EXPIRED | DELETED deriving (Show, Read, Eq, Generic)
derivePersistField "EntityStatus"

instance GQLType EntityStatus where
  type KIND EntityStatus = ENUM

data Locale = EN_US | ES_US | ES_BO deriving (Eq, Generic)

instance Show Locale where
  show EN_US = "en_US"
  show ES_US = "es_US"
  show ES_BO = "es_BO"

instance Read Locale where
  readsPrec _ "en_US" = [(EN_US, "en_US")]
  readsPrec _ "es_US" = [(ES_US, "es_US")]
  readsPrec _ "es_BO" = [(ES_BO, "es_BO")]

instance GQLType Locale where
  type KIND Locale = ENUM

derivePersistField "Locale"
