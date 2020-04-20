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

module Graphql.Maintenance.TaskTrigger.EventTrigger (
      EventTrigger
    , EventTriggerArg
    , getEventTriggerByIdResolver_
    , listEventTriggerResolver
    , saveEventTriggerResolver
) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
--import Graphql.Utils
import Data.Time

data EventTrigger = EventTrigger { eventTriggerId :: Int
                                 , name :: Text
                                 , description :: Maybe Text
                                 , createdDate :: Text
                                 , modifiedDate :: Maybe Text
                                 } deriving (Generic, GQLType)

data EventTriggerArg = EventTriggerArg { eventTriggerId :: Int
                                       , name :: Text
                                       , description :: Maybe Text
                                       } deriving (Generic)

getEventTriggerByIdResolver_ eventTriggerId arg = lift $ do
                                      eventTrigger <- runDB $ getJustEntity eventTriggerId
                                      return $ toEventTriggerQL eventTrigger

--listEventTriggerResolver :: () -> Res e Handler [EventTrigger]
listEventTriggerResolver _ = lift $ do
                               eventTrigger <- runDB $ selectList [] []
                               return $ P.map toEventTriggerQL eventTrigger

--saveEventTriggerResolver :: EventTriggerArg -> MutRes e Handler EventTrigger
saveEventTriggerResolver arg = lift $ createOrUpdateEventTrigger arg

createOrUpdateEventTrigger :: EventTriggerArg -> Handler EventTrigger
createOrUpdateEventTrigger eventTriggerArg = do
                          let EventTriggerArg {..} = eventTriggerArg
                          now <- liftIO getCurrentTime
                          entityId <- if eventTriggerId > 0 then
                                          do
                                            let eventTriggerKey = (toSqlKey $ fromIntegral $ eventTriggerId)::EventTrigger_Id
                                            _ <- runDB $ update eventTriggerKey [ EventTrigger_Name =. name
                                                                                , EventTrigger_Description =. description
                                                                                , EventTrigger_ModifiedDate =. Just now
                                                                                ]
                                            return eventTriggerKey
                                         else do
                                            eventTriggerKey <- runDB $ insert $ fromEventTriggerQL eventTriggerArg now Nothing
                                            return eventTriggerKey
                          eventTrigger <- runDB $ getJustEntity entityId
                          return $ toEventTriggerQL eventTrigger

toEventTriggerQL :: Entity EventTrigger_ -> EventTrigger
toEventTriggerQL (Entity eventTriggerId eventTriggerArg) = EventTrigger { eventTriggerId = fromIntegral $ fromSqlKey eventTriggerId
                                                                        , name = eventTrigger_Name
                                                                        , description = eventTrigger_Description
                                                                        , createdDate = fromString $ show eventTrigger_CreatedDate
                                                                        , modifiedDate = m
                                                                        }
                          where
                            EventTrigger_ {..} = eventTriggerArg
                            m = case eventTrigger_ModifiedDate of
                                  Just d -> Just $ fromString $ show d
                                  Nothing -> Nothing

fromEventTriggerQL :: EventTriggerArg -> UTCTime -> Maybe UTCTime -> EventTrigger_
fromEventTriggerQL (EventTriggerArg {..}) cd md = EventTrigger_ { eventTrigger_Name = name
                                                                , eventTrigger_Description = description
                                                                , eventTrigger_CreatedDate = cd
                                                                , eventTrigger_ModifiedDate = md
                                                                }
