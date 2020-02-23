{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handler.Home where

import Import
import Text.Hamlet          (hamletFile)
import URI.ByteString.Extension (toText)
-- getHomeR :: Handler Html
-- getHomeR = homePage
getRemoteLoginR :: Handler ()
getRemoteLoginR = redirect ("/auth/page/inventoty-auth-provider/forward" :: Text)

getForwardAdminR :: Handler ()
getForwardAdminR = redirect ("/admin/index.html" :: Text)

getRemoteLogoutR :: Handler ()
getRemoteLogoutR = do
                    render <- getUrlRender
                    App {appSettings} <- getYesod
                    let AppSettings {oauth2Conf} = appSettings
                    let Oauth2Config {logoutEndpoint} = oauth2Conf
                    let url = render RootR
                    redirect ((toText logoutEndpoint) <> "?callbackUrl=" <> url <> "auth/logout" :: Text)

getRootR :: Handler Html
getRootR = do
              maid <- maybeAuthId
              response <- case maid of
                           Nothing -> redirect RemoteLoginR
                           Just _ -> redirect ForwardAdminR
              return response

getHomeR :: Handler Html
getHomeR = do
        maid <- maybeAuthId
        response <- case maid of
                     Nothing -> redirect RemoteLoginR
                     Just _ -> defaultLayout
                                     [whamlet|
                                         <p>Your current auth ID: #{show maid}
                                         <p>
                                             <a href=@{RemoteLogoutR}>Logout
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

--getIndexR :: Handler ()
--getIndexR = redirect ("/admin/test"::Text)

--htmlType :: ContentType
--htmlType = "text/html"

--getIndexR :: Handler TypedContent
--getIndexR = do
--                addHeader "X-Frame-Options" "sameorigin"
--                return $ TypedContent htmlType $ toContent ("<h6>redirect</h6>"::Text)
