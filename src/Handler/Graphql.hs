{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Graphql where

import Import
import Graphql.Root (api, apiDoc)
import Data.Morpheus.Types (GQLRequest)

postGraphqlR :: Handler Value
postGraphqlR = do
                request <- requireCheckJsonBody::Handler GQLRequest
                response <- api request
                sendStatusJSON status200 response

jsonType :: ContentType
jsonType = "text/plain"

getGraphqlR :: Handler TypedContent
getGraphqlR = do
                let result = apiDoc
                return $ TypedContent jsonType $ toContent $ result


--postHomeR :: Handler TypedContent
--postHomeR = do
--             r <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
--             let [x] = r
--             result <- liftIO $ api $ B.pack $ T.unpack x
--              result <- liftIO $ api $ B.pack $ T.unpack "query{ deity { fullName } }"
--              () <- do
--                     $logDebug $ T.pack $ B.unpack request
--                     return ()
--             return $ TypedContent jsonType $ toContent result
