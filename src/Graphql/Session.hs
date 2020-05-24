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
import Text.RawString.QQ
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType, lift, Res, MutRes)
import Database.Persist.Sql (rawSql, toSqlKey, fromSqlKey, Single(..))
import Prelude as P
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Set as S
import Graphql.Utils
import Data.Time

data Session = Session { authId :: Text
                       , username :: Text
                       , email :: Text
                       , firstName :: Text
                       , lastName :: Text
                       , locale :: Text
                       , permissions :: [Text]
                       } deriving (Generic, GQLType)

permissionsSql :: Text
permissionsSql = [r|
select case
           when r.role_id is null
               then concat('DEFAULT_ROLE.', p.name)
           else concat(upper(r.name), '.', p.name) end as permission
from (
         select distinct p.*, rp.role_id
         from t_privilege p
                  join t_role_privilege rp on (rp.privilege_id = p.privilege_id)
         where rp.role_id in (select role_id from t_user_role where user_id = ?)
         union
         select *, null as role_id
         from t_privilege
         where privilege_id in (select t_user_privilege.privilege_id from t_user_privilege where user_id = ?)
     ) p
         left join t_role r on r.role_id = p.role_id
order by r.name, p.name
|]
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
                                     permissions <- getPermissions userId
                                     return $ toSessionQL userKey user person (P.map (\p -> unSingle p) permissions)

getPermissions :: User_Id -> Handler [Single Text]
getPermissions userId = do
                         permissions <- runDB $ rawSql permissionsSql [toPersistValue userId, toPersistValue userId]
                         return permissions
-- toSessionQL :: Entity User_ -> Session
toSessionQL authId User_ {..} Person_ {..} p = Session { authId = T.pack $ show authId
                                                       , username = user_Username
                                                       , email = user_Email
                                                       , firstName = person_FirstName
                                                       , lastName = person_LastName
                                                       , locale = T.pack $ show user_Locale
                                                       , permissions = p
                                                       }
--                                where
--                                  User_ {..} = user
