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
postUserR :: Handler Value
postUserR = do
            user <- (requireJsonBody :: Handler User)
            newUser <- runDB $ insertEntity user
            returnJson newUser

-- UPDATE USER ENDPOINT
putUserR :: Handler Value
putUserR = do
            user <- (requireJsonBody :: Handler User)
--             let x :: Key User
--                 x = User
--             userId <- runDB $ insert $ user
--             newUser <- runDB $ updateGet user [UserUsername =. "nueww"]
            returnJson user

