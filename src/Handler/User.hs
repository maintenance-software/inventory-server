{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Handler.User where

import Crypto.BCrypt
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified DataTransfer.Person as DT
import Import



-- GETUSER
getCreateUserR :: Handler Value
getCreateUserR = do
                    returnJson (DT.Person Nothing "nss" "ss" "sss" "ssa" Nothing [DT.Contact Nothing "asda" "sss"])


-- CREATE USER ENDPOINT
postCreateUserR :: Handler Value
postCreateUserR = do
                    user <- (requireJsonBody :: Handler User)
                    user1 <- liftIO $ encryptPassword user
                    newUser <- runDB $ insertEntity user1
                    returnJson newUser

-- UPDATE USER ENDPOINT
putUpdateUserR :: UserId -> Handler Value
putUpdateUserR userId= do
                        user <- (requireJsonBody :: Handler User)
                        let User username email _ enabled = user
                        newUser <- runDB $ updateGet userId [UserUsername =. username,
                                                             UserEmail =. email,
                                                             UserEnabled =. enabled
                                                            ]
--                         sendStatusJSON noContent204 (object [])
                        returnJson newUser

-- User username password enabled ident
encryptPassword:: User -> IO User
encryptPassword (User a b c d) = do
                                    p <- liftIO $ getPasswordIO b
                                    return (User a p c d)

getPasswordIO:: Text->IO Text
getPasswordIO password = do
                    p <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (B.pack (T.unpack password))
                    return ( case p of
                             Nothing -> ""
                             Just b -> T.pack $ B.unpack b
                            )