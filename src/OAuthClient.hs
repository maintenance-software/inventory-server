{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module OAuthClient ( oauth2Client ) where

import Import.NoFoundation
import Yesod.Auth.OAuth2.Prelude
import qualified Data.Text as T

newtype User = User Text

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> o .: "sub"

pluginName :: Text
pluginName = "inventoty-auth-provider"

oauth2Client :: YesodAuth m => Oauth2Config -> AuthPlugin m
oauth2Client Oauth2Config {..} =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        (User userId, userResponse) <- authGetProfile pluginName manager token userInfoEndpoint

        pure Creds { credsPlugin = pluginName
                   , credsIdent = userId
                   , credsExtra = setExtra token userResponse
                   }
  where
    oauth2 = OAuth2 { oauthClientId = clientId
                    , oauthClientSecret = clientSecret
                    , oauthOAuthorizeEndpoint = authorizeEndpoint `withQuery` [ scopeParam "," scopes ]
                    , oauthAccessTokenEndpoint = accessTokenEndpoint
                    , oauthCallback = Nothing
                    }

{--

https://pbrisbin.com/tags/yesod/

DB CONNECT
==========
$> psql -U inventory_user -h 192.168.0.100 -d inventory_db

REQUEST AUTHORIZE
=================
curl -i -X GET \
   -H "Authorization:Bearer eyJlbmMiOiJBMTI4R0NNIiwiYWxnIjoiUlNBLU9BRVAiLCJraWQiOiIyMDE5LTA5LTE1VDAxOjMwOjAyLjcwMTM4NjAwM1oifQ.Oj5LjVqtjumRvmJ_elTJTCRu9lihmT9R--zTo5N6L-KFJCTnVYvS-QXWVAIaO38L1BHByAqwWwQ9Lf3fGoTHPjZbpdzN9MAhh0yLNvfBT75pYt9gTBLpjwk_BU9hSAtj0QvZZtcGSKaIUcmbAx5oGThBqD5l9F7utAjWrKYXkA4.Hy12ubuIsg3YpL8A.MtWBecFpDiVTB7Pafff4xqYUR57ohMwlB52dINuxtzqtnJSFsnnN8s-6HqJX7VtIZwxrPDcbgo-2B4vSqtMoaORWQg0ChHflzPHY8FyY6oi3rP4iM6yjZdMsWoMbNLUi5jXrPkbN9o6-a12ljoO9M52q9Vket8Sp3hgDsEVaLa6yjDxrb5yAnDVMiSM0.C59PsbBQpqiDjpzpvAsriA" \
 'http://192.168.0.100:4200/oauth/authorize?client_id=app&client_secret=appsecret&scope=openid&state=somerandomstate&response_type=code&redirect_uri=http%3A%2F%2Flocalhost%3A8080%2Fapp'


TOKEN REQUEST
=============
curl -i -X POST \
   -H "Content-Type:application/x-www-form-urlencoded" \
   -d "code=0f284f9e10a5b05d" \
   -d "scope=openid" \
   -d "client_id=app" \
   -d "client_secret=appsecret" \
   -d "grant_type=authorization_code" \
   -d "redirect_uri=http://localhost:8080/app" \
 'http://192.168.0.100:4200/oauth/token'

USER INFO
=========
curl -i -X POST \
   -H "Content-Type:application/x-www-form-urlencoded" \
   -H "Authorization:Bearer eyJlbmMiOiJBMTI4R0NNIiwiYWxnIjoiUlNBLU9BRVAiLCJraWQiOiIyMDE5LTA5LTE1VDAxOjMwOjAyLjcwMTM4NjAwM1oifQ.Oj5LjVqtjumRvmJ_elTJTCRu9lihmT9R--zTo5N6L-KFJCTnVYvS-QXWVAIaO38L1BHByAqwWwQ9Lf3fGoTHPjZbpdzN9MAhh0yLNvfBT75pYt9gTBLpjwk_BU9hSAtj0QvZZtcGSKaIUcmbAx5oGThBqD5l9F7utAjWrKYXkA4.Hy12ubuIsg3YpL8A.MtWBecFpDiVTB7Pafff4xqYUR57ohMwlB52dINuxtzqtnJSFsnnN8s-6HqJX7VtIZwxrPDcbgo-2B4vSqtMoaORWQg0ChHflzPHY8FyY6oi3rP4iM6yjZdMsWoMbNLUi5jXrPkbN9o6-a12ljoO9M52q9Vket8Sp3hgDsEVaLa6yjDxrb5yAnDVMiSM0.C59PsbBQpqiDjpzpvAsriA" \
 'http://192.168.0.100:4200/connect/userinfo'

 --}
