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


module Handler.GraphqlTest (api) where


import qualified Data.ByteString.Lazy.Char8 as B

import           GHC.Generics
import           Control.Monad.Except       (ExceptT (..))
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (GQLRootResolver (..), IORes, GQLType(..), Undefined(..), liftEitherM, liftM)
import           Data.Morpheus.Kind     (OBJECT)
import           Data.Text                  (Text)
import           Data.ByteString
import           Handler.Deity  (Deity (..), dbDeity, fetchDeity)
import           Database.Persist.Sql (toSqlKey, fromSqlKey)
import           Import hiding (liftM)
-- importGQLDocumentWithNamespace "schema.gql"

data QueryQL m = QueryQL { deity :: DeityArgs -> m Deity } deriving (Generic, GQLType)
data DeityArgs = DeityArgs { name :: Text, mythology :: Maybe Text } deriving (Generic)

-- resolveDeity :: DeityArgs -> IORes Deity
-- resolveDeity args = queryResolver $ dbDeity  (name args) (mythology args)

resolveDeity :: DeityArgs -> IORes e Deity
resolveDeity DeityArgs { name, mythology } = liftM $ dbDeity name mythology

rootResolver :: GQLRootResolver IO () QueryQL Undefined Undefined
rootResolver = GQLRootResolver { queryResolver = QueryQL {deity = resolveDeity}
                               , mutationResolver = Undefined
                               , subscriptionResolver = Undefined
                               }

api :: ByteString -> IO ByteString
api = interpreter rootResolver
