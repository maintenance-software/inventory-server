{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuasiQuotes           #-}


module Graphql.Root (api, apiDoc) where

import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Generics
import           Control.Monad.Except       (ExceptT (..))
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (GQLRootResolver (..), IORes, GQLType(..), Undefined(..), liftEither, lift, Res, MutRes, GQLRequest, GQLResponse)
import           Data.Morpheus.Kind     (OBJECT)
import           Data.Morpheus.Document (toGraphQLDocument)
import           Data.Text                  (Text)
import           Data.ByteString
import           Graphql.Deity  (Deity (..), dbDeity, fetchDeity)
import           Database.Persist.Sql (toSqlKey, fromSqlKey)
import           Import
import           Graphql.Privilege
import           Graphql.Role
-- importGQLDocumentWithNamespace "schema.gql"

data QueryQL m = QueryQL { deity :: DeityArgs -> m Deity
                         , privileges :: Privileges (Res () Handler)
                         , roles :: Roles (Res () Handler)
                         } deriving (Generic, GQLType)

newtype Mutation m = Mutation {
    savePrivilege :: Privilege -> m Privilege
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs { name :: Text, mythology :: Maybe Text } deriving (Generic)


resolveMutation::Mutation (MutRes () Handler)
resolveMutation = Mutation { savePrivilege = resolveSavePrivilege }


-- BASE EXAMPLE
-- https://github.com/dnulnets/haccessability
dbFetchDeity:: Text -> Handler Deity
dbFetchDeity name = do
                     let userId = (toSqlKey 3)::User_Id
                     deity <- runDB $ getEntity userId
                     return $ Deity {fullName = "dummy", power = Just "Shapeshifting"}

resolveDeity :: DeityArgs -> Res e Handler Deity
resolveDeity DeityArgs { name, mythology } = lift $ dbFetchDeity name

-- | The query resolver
resolveQuery::QueryQL (Res () Handler)
resolveQuery = QueryQL {  deity = resolveDeity, privileges = resolvePrivilege, roles = resolveRole }

rootResolver :: GQLRootResolver Handler () QueryQL Mutation Undefined
rootResolver = GQLRootResolver { queryResolver = resolveQuery
                               , mutationResolver = resolveMutation
                               , subscriptionResolver = Undefined
                               }

-- | Compose the graphQL api
api:: GQLRequest -> Handler GQLResponse
api r = do
         interpreter rootResolver r

apiDoc = toGraphQLDocument $ Just rootResolver
