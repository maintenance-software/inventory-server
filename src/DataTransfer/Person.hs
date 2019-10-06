{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DataTransfer.Person where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import DataTransfer.User

data Person = Person {  personId :: Int
                      , firstName :: Text
                      , lastName :: Text
                      , documentType :: Text
                      , documentId :: Text
                      , address :: Maybe Address
                      , contactInfo :: [Contact]
                      , account :: Maybe User
                     } deriving (Generic, Show)

data Contact = Contact {  contactId :: Int
                        , contact :: Text
                        , contactType :: Text
                       } deriving (Generic, Show)

data Address = Address {  addressId :: Int
                        , street1 :: Text
                        , street2 :: Text
                        , street3 :: Text
                        , zip :: Text
                        , city :: Text
                        , state :: Text
                        , country :: Text
                       } deriving (Generic, Show)

instance ToJSON Person where
instance FromJSON Person

instance ToJSON Contact where
instance FromJSON Contact

instance ToJSON Address where
instance FromJSON Address

-- PERSON GETTERS
getPersonId Person {..} = personId
getFirstName Person {..} = firstName
getLastName Person {..} = lastName
getDocumentType Person {..} = documentType
getDocumentId Person {..} = documentId
getAddress Person {..} = address
getContactInfo Person {..} = contactInfo

--ADDRESS GETTERS
getAddressId Address {..} = addressId
getStreet1 Address {..} = street1
getStreet2 Address {..} = street2
getStreet3 Address {..} = street3
getZip Address {..} = zip
getCity Address {..} = city
getState Address {..} = state
getCountry Address {..} = country

-- CONTACT GETTERS
getContactId Contact {..} = contactId
getContact Contact {..} = contact
getContactType Contact {..} = contactType


-- instance ToJSON Person where
--             toJSON Person {..} = object [  "personId" .= personId
--                                          , "firstName" .= firstName
--                                          , "lastName" .= lastName
--                                          , "documentType" .= documentType
--                                          , "documentId" .= documentId
--                                          , "address" .= address
--                                          , "contact" .= contact
--                                         ]

-- instance ToJSON Contact where
--             toJSON Contact {..} = object [  "contactId" .= contactId
--                                           , "contact" .= contact
--                                           , "contactType" .= contactType
--                                          ]

-- instance ToJSON Address where
--             toJSON Address {..} = object [  "addressId" .= addressId
--                                           , "street1" .= street1
--                                           , "street2" .= street2
--                                           , "street3" .= street3
--                                           , "zip" .= zip
--                                           , "city" .= city
--                                           , "state" .= state
--                                           , "country" .= country
--                                          ]
