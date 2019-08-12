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

import Import

-- CREATE USER ENDPOINT
postCreateUserR :: Handler Value
postCreateUserR = do
                    user <- (requireJsonBody :: Handler User)
                    newUser <- runDB $ insertEntity user
                    returnJson newUser

-- UPDATE USER ENDPOINT
putUpdateUserR :: UserId -> Handler Value
putUpdateUserR userId= do
                        user <- (requireJsonBody :: Handler User)
                        _ <- runDB $ replace userId user
                        newUser <- runDB $ getJustEntity userId
                        returnJson newUser
--                         sendStatusJSON noContent204 (object [])