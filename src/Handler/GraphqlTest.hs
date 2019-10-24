{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.GraphqlTest (api) where


import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (GQLRootResolver (..), IORes)
import           Data.Text                  (Text)
import           Data.ByteString

importGQLDocumentWithNamespace "schema.gql"

rootResolver :: GQLRootResolver IO () () (Query IORes) () ()
rootResolver =
  GQLRootResolver
    {queryResolver = return Query {queryDeity}, mutationResolver = pure (), subscriptionResolver = pure ()}
  where
    queryDeity QueryDeityArgs {queryDeityArgsName} = pure Deity {deityName, deityPower}
      where
        deityName _ = pure "Morpheus"
        deityPower _ = pure (Just "Shapeshifting")

api :: ByteString -> IO ByteString
api = interpreter rootResolver
