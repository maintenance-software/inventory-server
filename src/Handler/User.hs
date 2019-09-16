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

module Handler.User where

import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Crypto.BCrypt
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified DataTransfer.Person as DT
import Import
import qualified DataTransfer.User as U



-- GETUSER
-- getCreateUserR :: Handler Value
-- getCreateUserR = do
--                    returnJson (DT.Person Nothing "nss" "ss" "sss" "ssa" Nothing [DT.Contact Nothing "asda" "sss"])

-- GET USER BY ID
getFindByIdR :: UserId -> Handler Value
getFindByIdR userId = do
                user <- runDB $ getJustEntity userId
                returnJson $ buildUserResponse user


-- LIST USERS ENDPOINT
getUserR :: Handler Value
getUserR = do
            users <- runDB $ selectList [] [Asc UserId]
            returnJson users

-- CREATE USER ENDPOINT
postUserR :: Handler Value
postUserR = putUserR

-- UPDATE USER ENDPOINT
putUserR :: Handler Value
putUserR = do
            user <- requireCheckJsonBody :: Handler U.User
            userId <- if (U.getUserId user) > 0 then
                        do
                         let userKey = (toSqlKey $ fromIntegral $ U.getUserId user)::UserId
                         _ <- runDB $ update userKey [  UserUsername =. U.getUsername user
                                                      , UserEmail =. U.getEmail user
                                                      , UserActive =. U.getActive user
                                                     ]
                         return userKey
                      else do
                            userEncrypeted <- liftIO $ encryptPassword $ fromUserDT user
                            userKey <- runDB $ insert userEncrypeted
                            return userKey
            response <- getFindByIdR userId
            returnJson response

-- User username password enabled ident
encryptPassword:: User -> IO User
encryptPassword (User a b c d) = do
                                    p <- liftIO $ getPasswordIO c
                                    return (User a b p d)

getPasswordIO:: Text->IO Text
getPasswordIO password = do
                    p <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (B.pack (T.unpack password))
                    return ( case p of
                             Nothing -> ""
                             Just b -> T.pack $ B.unpack b
                            )


buildUserResponse :: Entity User -> U.User
buildUserResponse (Entity userId user) = U.User {  userId = fromIntegral $ fromSqlKey userId
                                                 , username = a
                                                 , email = b
                                                 , password = "######"
                                                 , active = d
                                                 , roles = []
                                                 , privileges = []
                                                }
                                  where
                                    User a b _ d = user


fromUserDT :: U.User -> User
fromUserDT U.User {..} = User username email password active





-- postUserR :: Handler Value
-- postUserR = do
--                    user <- requireJsonBody :: Handler User
--                    user1 <- liftIO $ encryptPassword user
--                    newUser <- runDB $ insertEntity user1
--                    returnJson newUser