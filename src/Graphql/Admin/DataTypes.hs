{-# LANGUAGE CPP                   #-}
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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Graphql.Admin.DataTypes where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..))
import Graphql.Utils (Page, PageArg, EntityIdArg, EntityChangeStatusArg)
import Graphql.Category
import Graphql.Admin.Privilege
import Graphql.Admin.Role


-- PERSON DATA TYPES
data Person o = Person { personId :: Int
                       , firstName :: Text
                       , lastName :: Text
                       , documentType :: Text
                       , documentId :: Text
                       , createdDate :: Text
                       , modifiedDate :: Maybe Text
                       , address :: () -> o () Handler (Maybe Address)
                       , contactInfo :: () -> o () Handler [ContactInfo]
--                       , user :: Maybe (() -> o () Handler (User o))
                       } deriving (Generic, GQLType)

data Persons o = Persons { person :: EntityIdArg -> o () Handler (Person o)
                         , page :: PageArg -> o () Handler (Page (Person o))
                         , createUpdatePerson :: PersonArg -> o () Handler (Person o)
                         } deriving (Generic, GQLType)

data ContactInfo = ContactInfo { contactId :: Int
                               , contact :: Text
                               , contactType :: Text
                               , createdDate :: Text
                               , modifiedDate :: Maybe Text
                               } deriving (Generic, GQLType)

data Address = Address {  addressId :: Int
                        , street1 :: Text
                        , street2 :: Text
                        , street3 :: Text
                        , zip :: Text
                        , city :: Text
                        , state :: Text
                        , country :: Text
                        , createdDate :: Text
                        , modifiedDate :: Maybe Text
                       } deriving (Generic, GQLType)

-- Person Graphql Arguments
--data PersonAddressArg = PersonAddressArg {address :: Maybe AddressArg} deriving (Generic, GQLType)
--data PersonContactInfoArg = PersonContactInfoArg {contactInfo :: Maybe [ContactInfoArg]} deriving (Generic, GQLType)
--data PersonUserArg = PersonUserArg {user :: Maybe UserArg} deriving (Generic, GQLType)
data PersonArg = PersonArg { personId :: Int
                           , firstName :: Text
                           , lastName :: Text
                           , documentType :: Text
                           , documentId :: Text
                           , address :: Maybe AddressArg
                           , contactInfo :: [ContactInfoArg]
--                           , user :: Maybe UserArg
                           } deriving (Generic)

instance GQLType PersonArg where
    type  KIND PersonArg = INPUT_OBJECT
    description = const $ Just $ pack "The item that holds the person information"

data AddressArg = AddressArg { addressId :: Int
                             , street1 :: Text
                             , street2 :: Text
                             , street3 :: Text
                             , zip :: Text
                             , city :: Text
                             , state :: Text
                             , country :: Text
                             } deriving (Generic)

instance GQLType AddressArg where
    type  KIND AddressArg = INPUT_OBJECT
    description = const $ Just $ pack "The item that holds the address information"

data ContactInfoArg = ContactInfoArg { contactId :: Int
                                     , contact :: Text
                                     , contactType :: Text
                                     } deriving (Generic)

instance GQLType ContactInfoArg where
    type  KIND ContactInfoArg = INPUT_OBJECT
    description = const $ Just $ pack "The item that holds the contact Info information"


-- USER DATA TYPES
data User o = User { userId :: Int
                   , username :: Text
                   , email :: Text
                   , status :: Text
                   , locale :: Text
                   , expiration :: Bool
                   , newPasswordRequired :: Bool
                   , person :: () -> o () Handler (Person o)
                   , roles :: () -> o () Handler [Role o]
                   , privileges :: () -> o () Handler [Privilege]
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   } deriving (Generic, GQLType)

data Users o = Users { user :: EntityIdArg -> o () Handler (User o)
                     , page :: PageArg -> o () Handler (Page (User o))
                     , createUpdateUser :: UserArg -> o () Handler (User o)
                     , resetPassword :: EntityIdArg -> o () Handler Text
                     , changePassword :: ChangePasswordArg -> o () Handler Bool
                     , updatePassword :: UpdatePasswordArg -> o () Handler Bool
                     } deriving (Generic, GQLType)

data ChangePasswordArg = ChangePasswordArg { userId:: Int, password :: Text, newPassword :: Text} deriving (Generic)
data UpdatePasswordArg = UpdatePasswordArg { userId:: Int, password :: Text } deriving (Generic)
-- User Graphql Arguments
data UserArg = UserArg { userId :: Int
                       , username :: Text
                       , email :: Text
                       , status :: Text
                       , locale :: Text
                       , expiration :: Bool
                       , person :: PersonArg
                       , roleIds :: [Int]
                       , privilegeIds :: [Int]
                       } deriving (Generic)

--data UserRoleArg = UserRoleArg {roleIds :: Maybe [Int]} deriving (Generic, GQLType)
--data UserPrivilegeArg = UserPrivilegeArg { privilegeIds :: Maybe [Int]} deriving (Generic, GQLType)
