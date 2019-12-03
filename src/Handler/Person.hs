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

module Handler.Person where

import qualified DataTransfer.Person as DT
import Import
import Business.PersonBizFunc (getPersonByIdBizFunc, listPersonsBizFunc, createOrUpdatePersonBizFunc, listPersonsUsersBizFunc)

-- GET PERSON BY ID
getPersonIdR :: Person_Id -> Handler Value
getPersonIdR personId = do
                        person <- getPersonByIdBizFunc personId
                        returnJson person

-- LIST PERSONS ENDPOINT
getPersonR :: Handler Value
getPersonR = do
              persons <- listPersonsUsersBizFunc
              returnJson persons

-- CREATE OR UPDATE PERSON
postPersonR :: Handler Value
postPersonR = putPersonR

-- CREATE OR UPDATE PERSON
putPersonR :: Handler Value
putPersonR = do
               person <- requireCheckJsonBody :: Handler DT.Person
               response <- createOrUpdatePersonBizFunc person
               returnJson response

-- postPersonR :: Handler Value
-- postPersonR = do
--                person <- (requireJsonBody :: Handler DT.Person)
--                let (DT.Person personId _ _ _ _ _) = person
--                newUser <- runDB $ get ((toSqlKey $ fromIntegral personId)::UserId)
--                returnJson newUser
