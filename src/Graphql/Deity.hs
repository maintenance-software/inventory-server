{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Graphql.Deity
  ( Deity(..)
  , dbDeity
  , fetchDeity
  , NoDeity(..)
  , TestArg(..)
  ) where

import           Data.Morpheus.Kind     (OBJECT)
import           Data.Morpheus.Types    (GQLType (..), lift, Res)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Database.Persist.Sql (toSqlKey, fromSqlKey)
import           Import
-- import Application (handler)
data NoDeity = NoDeity
  { noFullName :: Text -- Non-Nullable Field
  , nopower    :: Maybe Text -- Nullable Field
  } deriving (Generic)

instance GQLType NoDeity where
  type KIND NoDeity = OBJECT
  description _ = Just "Custom Description for Client Defined User Type"

data TestArg = TestArg { yourFullName :: Text } deriving (Generic)

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  , tests :: TestArg -> Res () Handler NoDeity
  } deriving (Generic)

instance GQLType Deity where
  type KIND Deity = OBJECT
  description _ = Just "Custom Description for Client Defined User Type"

dbDeity :: Text -> Maybe Text -> IO Deity
dbDeity name _ = do
                 let userId = (toSqlKey 3)::User_Id
--                  user <- runDB $ getJustEntity userId
                 return $ Deity {fullName = "Hi, " <> name, power = Just "Shapeshifting"}

toUser :: Entity User_ -> Deity
toUser _ = Deity {fullName = "Hi, ", power = Just "Shapeshifting"}

fetchDeity :: Handler Deity
fetchDeity = do
              let userId = (toSqlKey 3)::User_Id
              user <- runDB $ getJustEntity userId
              return $ Deity {fullName = "dummy", power = Just "Shapeshifting"}

