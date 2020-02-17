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

module Graphql.Session (Session, getUserSessionResolver, toSessionQL) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Set as S
import Graphql.Utils
import Data.Time
import Graphql.Category

data Session = Session { username :: Text
                       , email :: Text
                       , firstName :: Text
                       , lastName :: Text
                       , language :: Text
                       } deriving (Generic, GQLType)

-- Query Resolvers
getUserSessionResolver :: () -> Res e Handler Session
getUserSessionResolver () = lift $ do
                                     muid <- maybeAuthId
--                                     () <- case muid of
--                                              Nothing -> $logWarn   "Test log"
--                                              Just a -> $logWarn   a
                                     let userKey = case muid of
                                                    Nothing -> 0
                                                    Just a -> read $ P.filter  (\c -> isDigit c) (T.unpack a) :: Int
                                     let userId = (toSqlKey $ fromIntegral $ userKey)::User_Id
                                     Entity _ user <- runDB $ getJustEntity userId
                                     let User_ {..} = user
                                     Entity _ person <- runDB $ getJustEntity user_PersonId
                                     return $ toSessionQL user person

-- toSessionQL :: Entity User_ -> Session
toSessionQL User_ {..} Person_ {..} = Session { username = user_Username
                                              , email = user_Email
                                              , firstName = person_FirstName
                                              , lastName = person_LastName
                                              , language = T.pack $ show user_Language
                                              }
--                                where
--                                  User_ {..} = user
