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

module Graphql.Asset.Unit (
    Unit
  , UnitArg
  , listUnitResolver
  , saveUnitResolver
  , toUnitQL
  , dbFetchUnitById
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Graphql.Utils
import Data.Time

data Unit = Unit { unitId :: Int
                 , key :: Text
                 , label :: Text
                 , createdDate :: Text
                 , modifiedDate :: Maybe Text
                 } deriving (Generic, GQLType)

data UnitArg = UnitArg { unitId :: Int
                       , key :: Text
                       , label :: Text
                       } deriving (Generic)

-- DB ACTIONS
dbFetchUnitById:: Unit_Id -> Handler Unit
dbFetchUnitById unitId = do
                          unit <- runDB $ getJustEntity unitId
                          return $ toUnitQL unit

dbFetchUnits :: Handler [Unit]
dbFetchUnits = do
                units <- runDB $ selectList [] []
                return $ P.map toUnitQL units

listUnitResolver :: () -> Res e Handler [Unit]
listUnitResolver _ = lift $ dbFetchUnits

-- Mutation

saveUnitResolver :: UnitArg -> MutRes e Handler Unit
saveUnitResolver arg = lift $ createOrUpdateUnit arg

createOrUpdateUnit :: UnitArg -> Handler Unit
createOrUpdateUnit unit = do
                let UnitArg {..} = unit
                now <- liftIO getCurrentTime
                entityId <- if unitId > 0 then
                                do
                                  let unitKey = (toSqlKey $ fromIntegral $ unitId)::Unit_Id
                                  _ <- runDB $ update unitKey [ Unit_Key =. key
                                                              , Unit_Label =. label
                                                              , Unit_ModifiedDate =. Just now
                                                              ]
                                  return unitKey
                               else do
                                  unitKey <- runDB $ insert $ fromUnitQL unit now Nothing
                                  return unitKey
                response <- dbFetchUnitById entityId
                return response

-- CONVERTERS
toUnitQL :: Entity Unit_ -> Unit
toUnitQL (Entity unitId unit) = Unit { unitId = fromIntegral $ fromSqlKey unitId
                                     , key = unit_Key
                                     , label = unit_Label
                                     , createdDate = fromString $ show unit_CreatedDate
                                     , modifiedDate = m
                                     }
                          where
                            Unit_ {..} = unit
                            m = case unit_ModifiedDate of
                                  Just d -> Just $ fromString $ show d
                                  Nothing -> Nothing

fromUnitQL :: UnitArg -> UTCTime -> Maybe UTCTime -> Unit_
fromUnitQL (UnitArg {..}) cd md = Unit_ { unit_Key = key
                                        , unit_Label = label
                                        , unit_CreatedDate = cd
                                        , unit_ModifiedDate = md
                                        }

