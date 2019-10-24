{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Hamlet          (hamletFile)
import Handler.GraphqlTest (api)

-- getHomeR :: Handler Html
-- getHomeR = homePage

getIndexR :: Handler TypedContent
getIndexR = getHomeR

jsonType :: ContentType
jsonType = "application/json"

getHomeR :: Handler TypedContent
getHomeR = do
          result <- liftIO $ api "query GetDeity { deity (name: \"Morpheus\") { fullName power } }"
          return $ TypedContent jsonType $ toContent $ result
 
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
