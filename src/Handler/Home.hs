{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Hamlet          (hamletFile)

getHomeR :: Handler Html
getHomeR = homePage

getIndexR :: Handler Html
getIndexR = homePage



homePage :: Handler Html
homePage = do
            master <- getYesod
            mmsg <- getMessage
            muser <- maybeAuthPair
            mcurrentRoute <- getCurrentRoute
            withUrlRenderer $(hamletFile "src/Views/index.hamlet")
