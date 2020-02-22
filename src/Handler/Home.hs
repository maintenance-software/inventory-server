{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Hamlet          (hamletFile)

-- getHomeR :: Handler Html
-- getHomeR = homePage
getForwardLoginR :: Handler ()
getForwardLoginR = redirect ("/auth/page/inventoty-auth-provider/forward" :: Text)

getForwardAdminR :: Handler ()
getForwardAdminR = redirect ("/admin/index.html" :: Text)

getIndexR :: Handler Html
getIndexR = do
              maid <- maybeAuthId
              response <- case maid of
                           Nothing -> redirect ForwardLoginR
                           Just _ -> redirect ForwardAdminR
              return response

--getIndexR :: Handler ()
--getIndexR = redirect ("/admin/test"::Text)

--htmlType :: ContentType
--htmlType = "text/html"

--getIndexR :: Handler TypedContent
--getIndexR = do
--                addHeader "X-Frame-Options" "sameorigin"
--                return $ TypedContent htmlType $ toContent ("<h6>redirect</h6>"::Text)

getHomeR :: Handler Html
getHomeR = do
        maid <- maybeAuthId
        response <- case maid of
                     Nothing -> redirect ForwardLoginR
                     Just _ -> defaultLayout
                                     [whamlet|
                                         <p>Your current auth ID: #{show maid}
                                         <p>
                                             <a href=@{AuthR LogoutR}>Logout
                                     |]
        return response
{--
        case maid of
             Nothing -> redirect ("auth/page/github/forward" :: Text)
             Just _ -> defaultLayout
                            [whamlet|
                                <p>Your current auth ID: #{show maid}
                                <p>
                                    <a href=@{AuthR LogoutR}>Logout
                            |]
--}
        -- redirect $ AuthR LoginR



{--
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]
--}

-- homePage :: Handler Html
-- homePage = do
--            master <- getYesod
--            mmsg <- getMessage
--            muser <- maybeAuthPair
--            mcurrentRoute <- getCurrentRoute
--            withUrlRenderer $(hamletFile "src/Views/index.hamlet")
