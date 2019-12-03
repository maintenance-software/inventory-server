{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Privilege where

import qualified Prelude as PP
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified DataTransfer.Privilege as P
import Import

-- GET PRIVILEGE BY ID
getPrivilegeIdR :: Privilege_Id -> Handler Value
getPrivilegeIdR privilegeId = do
                                privilege <- runDB $ getJustEntity privilegeId
                                returnJson $ buildPrivilegeResponse privilege

-- DELETE PRIVILEGE BY ID
deletePrivilegeIdR :: Privilege_Id -> Handler Value
deletePrivilegeIdR privilegeId = do
                                _ <- runDB $ delete privilegeId
                                sendResponseStatus status200 ("DELETED" :: Text)

-- LIST PRIVILEGES ENDPOINT
getPrivilegeR :: Handler Value
getPrivilegeR = do
                privileges <- runDB $ selectList ([] :: [Filter Privilege_]) []
                returnJson $ PP.map buildPrivilegeResponse privileges

-- CREATE PRIVILEGE ENDPOINT
postPrivilegeR :: Handler Value
postPrivilegeR = putPrivilegeR

-- UPDATE PRIVILEGE ENDPOINT
putPrivilegeR :: Handler Value
putPrivilegeR = do
                privilege <- requireCheckJsonBody :: Handler P.Privilege
                privilegeId <- if (P.getPrivilegeId privilege) > 0 then
                                do
                                  let privilegeKey = (toSqlKey $ fromIntegral $ P.getPrivilegeId privilege)::Privilege_Id
                                  _ <- runDB $ update privilegeKey [  Privilege_Name =. P.getPrivilegeName privilege
                                                                      , Privilege_Active =. P.getActive privilege
                                                                     ]
                                  return privilegeKey
                               else do
                                  privilegeKey <- runDB $ insert $ fromPrivilegeDT privilege
                                  return privilegeKey
                response <- getPrivilegeIdR privilegeId
                returnJson response

buildPrivilegeResponse :: Entity Privilege_ -> P.Privilege
buildPrivilegeResponse (Entity privilegeId privilege) = P.Privilege {  privilegeId = fromIntegral $ fromSqlKey privilegeId
                                                                     , privilegeName = a
                                                                     , description = b
                                                                     , active = c
                                                                    }
                                                      where
                                                        Privilege_ a b c = privilege


fromPrivilegeDT :: P.Privilege -> Privilege_
fromPrivilegeDT P.Privilege {..} = Privilege_ privilegeName description active
