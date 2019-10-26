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
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (queryResolver, Resolver (..), GQLRootResolver (..), IORes, GQLType (..))
import           Data.Morpheus.Kind     (OBJECT)
import           Data.Text                  (Text)
import           Data.ByteString
import           Handler.Deity  (Deity (..), dbDeity)


-- importGQLDocumentWithNamespace "schema.gql"

data Query = Query
  { deity :: DeityArgs -> IORes Deity
  } deriving (Generic)

instance GQLType Query where
  type KIND Query = OBJECT
  description _ = Just "Custom Description for Client Defined User Type"

data DeityArgs = DeityArgs
  { name      :: Text  -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic)


-- resolveDeity :: DeityArgs -> IORes Deity
-- resolveDeity args = queryResolver $ dbDeity  (name args) (mythology args)

rootResolver :: GQLRootResolver IO () () Query () ()
rootResolver = GQLRootResolver { queryResolver = return Query {deity}
                               , mutationResolver = return ()
                               , subscriptionResolver = return ()
                               }
            where
                deity DeityArgs{name, mythology} = pure Deity {fullName, power}
                  where
                          fullName = "Hi" <> name
                          power = Just "Shapeshifting"

api :: ByteString -> IO ByteString
api = interpreter rootResolver
