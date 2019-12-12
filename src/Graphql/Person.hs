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

module Graphql.Person (Persons, Person, resolvePerson, PersonMut, PersonArg, resolveSavePerson_) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Data.Time

data DummyArg = DummyArg {} deriving (Generic)

data Person = Person { personId :: Int
                     , firstName :: Text
                     , lastName :: Text
                     , documentType :: Text
                     , documentId :: Text
                     , createdDate :: Text
                     , modifiedDate :: Maybe Text
                     , address :: DummyArg -> Res () Handler (Maybe Address)
                     , contactInfo :: DummyArg -> Res () Handler [ContactInfo]
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

data Persons m = Persons { person :: GetEntityByIdArg -> m Person
                         , list :: ListArgs -> m [Person]
                         } deriving (Generic, GQLType)

-- Query Resolvers
resolvePerson :: Persons (Res () Handler)
resolvePerson = Persons {  person = findByIdResolver, list = listResolver}

findByIdResolver :: GetEntityByIdArg -> Res e Handler Person
findByIdResolver GetEntityByIdArg {..} = lift $ do
                                      let personEntityId = (toSqlKey $ fromIntegral $ entityId)::Person_Id
                                      person <- runDB $ getJustEntity personEntityId
                                      return $ toPersonQL person

resolveAddress :: Person_Id -> DummyArg -> Res e Handler (Maybe Address)
resolveAddress personId arg = lift $ do
                    addressMaybe <- runDB $ selectFirst [Address_PersonId ==. personId] []
                    let address = case addressMaybe of
                                    Nothing -> Nothing
                                    Just a -> Just $ toAddressQL a
                    return address

resolveContactInfo :: Person_Id -> DummyArg -> Res e Handler [ContactInfo]
resolveContactInfo personId arg = lift $ do
                                      contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                                      return $ P.map toContactQL contacts

listResolver :: ListArgs -> Res e Handler [Person]
listResolver ListArgs{..} = lift $ do
                                persons <- runDB $ selectList [] [Asc Person_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                                return $ P.map (\p -> toPersonQL p) persons
                         where
                          (page, size) = case pageable of
                                          Just (Pageable x y) -> (x, y)
                                          Nothing -> (1, 10)

toPersonQL :: Entity Person_ -> Person
toPersonQL (Entity personId person) = Person { personId = fromIntegral $ fromSqlKey personId
                                             , firstName = person_FirstName
                                             , lastName = person_LastName
                                             , documentType = person_DocumentType
                                             , documentId = person_DocumentId
                                             , createdDate = fromString $ show person_CreatedDate
                                             , modifiedDate = md
                                             , address = resolveAddress personId
                                             , contactInfo = resolveContactInfo personId
                                             }
                                 where
                                  Person_ {..} = person
                                  md = case person_ModifiedDate of
                                        Just d -> Just $ fromString $ show d
                                        Nothing -> Nothing

toContactQL :: Entity ContactInfo_ -> ContactInfo
toContactQL (Entity contactId contact) = ContactInfo { contactId = fromIntegral $ fromSqlKey contactId
                                                     , contact = contactInfo_Contact
                                                     , contactType = contactInfo_ContactType
                                                     , createdDate = fromString $ show contactInfo_CreatedDate
                                                     , modifiedDate = md
                                                     }
                                    where
                                      ContactInfo_ {..} = contact
                                      md = case contactInfo_ModifiedDate of
                                            Just d -> Just $ fromString $ show d
                                            Nothing -> Nothing

toAddressQL :: Entity Address_ -> Address
toAddressQL (Entity addressId address) = Address { addressId = fromIntegral $ fromSqlKey addressId
                                                 , street1 = address_Street1
                                                 , street2 = address_Street2
                                                 , street3 = address_Street3
                                                 , zip = address_Zip
                                                 , city = address_City
                                                 , state = address_State
                                                 , country = address_Country
                                                 , createdDate = fromString $ show address_CreatedDate
                                                 , modifiedDate = md
                                                 }
                                             where
                                                Address_ {..} = address
                                                md = case address_ModifiedDate of
                                                      Just d -> Just $ fromString $ show d
                                                      Nothing -> Nothing

-- Person Graphql Arguments
data PersonArg = PersonArg { personId :: Int
                           , firstName :: Text
                           , lastName :: Text
                           , documentType :: Text
                           , documentId :: Text
                           } deriving (Generic)

data AddressArg = AddressArg { addressId :: Int
                             , street1 :: Text
                             , street2 :: Text
                             , street3 :: Text
                             , zip :: Text
                             , city :: Text
                             , state :: Text
                             , country :: Text
                             } deriving (Generic)

data ContactInfoArg = ContactInfoArg { contactId :: Int
                                     , contact :: Text
                                     , contactType :: Text
                                     } deriving (Generic)
instance GQLType ContactInfoArg where
    type  KIND ContactInfoArg = INPUT_OBJECT
    description = const $ Just $ pack "The item that holds the contact Info information"

newtype ContactInfoArgWrapper = ContactInfoArgWrapper { contactInfo :: [ContactInfoArg] } deriving (Generic)

data PersonMut = PersonMut { personId :: Int
                           , firstName :: Text
                           , lastName :: Text
                           , documentType :: Text
                           , documentId :: Text
                           , createdDate :: Text
                           , modifiedDate :: Maybe Text
                           , address :: AddressArg -> MutRes () Handler Address
                           , contactInfo :: ContactInfoArgWrapper -> MutRes () Handler [ContactInfo]
                           } deriving (Generic, GQLType)

resolveSavePerson_ :: PersonArg -> MutRes e Handler PersonMut
resolveSavePerson_ arg = lift $ do
                                personId <- createOrUpdatePerson_ arg
                                person <- runDB $ getJustEntity personId
                                let Entity _ Person_ {..} = person
                                let md = case person_ModifiedDate of
                                          Just d -> Just $ fromString $ show d
                                          Nothing -> Nothing
                                return PersonMut { personId = fromIntegral $ fromSqlKey personId
                                                 , firstName = person_FirstName
                                                 , lastName = person_LastName
                                                 , documentType = person_DocumentType
                                                 , documentId = person_DocumentId
                                                 , createdDate = fromString $ show person_CreatedDate
                                                 , modifiedDate = md
                                                 , address = resolveSaveAddress personId
                                                 , contactInfo = resolveSaveContactInfo personId
                                                 }

resolveSaveAddress :: Person_Id -> AddressArg -> MutRes e Handler Address
resolveSaveAddress personId arg = lift $ do
                                          addressId <- createOrUpdateAddress personId arg
                                          addressEntity <- runDB $ getJustEntity addressId
                                          return $ toAddressQL addressEntity

resolveSaveContactInfo :: Person_Id -> ContactInfoArgWrapper -> MutRes e Handler [ContactInfo]
resolveSaveContactInfo personId ContactInfoArgWrapper {..} = lift $ do
                                          () <- createOrUpdateContactInfo personId contactInfo
                                          contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                                          return $ P.map toContactQL contacts

createOrUpdatePerson_ :: PersonArg -> Handler Person_Id
createOrUpdatePerson_ personArg = do
                               let PersonArg{..} = personArg
                               now <- liftIO getCurrentTime
                               personEntityId <- if personId > 0 then
                                            do
                                              let personKey = (toSqlKey $ fromIntegral personId)::Person_Id
                                              _ <- runDB $ update personKey [  Person_FirstName =. firstName
                                                                             , Person_LastName =. lastName
                                                                             , Person_DocumentType =. documentType
                                                                             , Person_DocumentId =. documentId
                                                                             , Person_ModifiedDate =. Just now
                                                                            ]
                                              return personKey
                                            else
                                              do
                                                personKey <- runDB $ insert (fromPersonQL_ personArg now Nothing)
                                                return personKey
                               return personEntityId

createOrUpdateAddress :: Person_Id -> AddressArg -> Handler Address_Id
createOrUpdateAddress personId address = do
                               let AddressArg {..} = address
                               now <- liftIO getCurrentTime
                               addressEntityId <- if addressId > 0 then
                                                   do
                                                     let addressId' = (toSqlKey $ fromIntegral addressId)::Address_Id
                                                     _ <- runDB $ update addressId' [  Address_Street1 =. street1
                                                                                     , Address_Street2 =. street2
                                                                                     , Address_Street3 =. street3
                                                                                     , Address_Zip =. zip
                                                                                     , Address_City =. city
                                                                                     , Address_State =. state
                                                                                     , Address_Country =. country
                                                                                     , Address_ModifiedDate =. Just now
                                                                                    ]
                                                     return addressId'
                                                  else
                                                   do
                                                     addressId' <- runDB $ insert (fromAddressQL_ personId address now Nothing)
                                                     return addressId'
                               return addressEntityId

createOrUpdateContactInfo :: Person_Id -> [ContactInfoArg] -> Handler ()
createOrUpdateContactInfo personId  contactInfo = do
                               now <- liftIO getCurrentTime
                               let c1 = P.filter (\ContactInfoArg {..} -> contactId <= 0)  $ contactInfo
                               let c2 = P.filter (\ContactInfoArg {..} -> contactId > 0)  $ contactInfo
                               contactIds <- runDB $ insertMany  $ [fromContactQL_ personId c now Nothing | c <- c1]
                               _ <- updateContact_ c2
                               return ()

updateContact_ [] = return ()
updateContact_ (x:xs)= do
                        let ContactInfoArg {..} = x
                        now <- liftIO getCurrentTime
                        let entityContactId = (toSqlKey $ fromIntegral contactId)::ContactInfo_Id
                        _ <- runDB $ update entityContactId [ ContactInfo_ContactType =. contactType
                                                            , ContactInfo_Contact =. contact
                                                            , ContactInfo_ModifiedDate =. Just now
                                                            ]
                        _ <- updateContact_ xs
                        return ()

fromPersonQL_ :: PersonArg -> UTCTime -> Maybe UTCTime -> Person_
fromPersonQL_ PersonArg {..} cd md = Person_ firstName lastName documentType documentId cd md

fromAddressQL_ :: Person_Id -> AddressArg -> UTCTime -> Maybe UTCTime -> Address_
fromAddressQL_ personId AddressArg {..} cd md = Address_ street1 street2 street3 zip city state country personId cd md

fromContactQL_ :: Person_Id -> ContactInfoArg -> UTCTime -> Maybe UTCTime -> ContactInfo_
fromContactQL_ personId ContactInfoArg {..} cd md = ContactInfo_ contactType contact personId cd md


{-

query {
  persons {
    person(entityId: 16) {
    personId
    firstName
    lastName
    createdDate
    modifiedDate
    address {
      addressId
      city
      country
      state
    }

    contactInfo {
      contactId
      contact
      contactType
    }
    }
  }
}


mutation {
  savePerson(personId:16, firstName: "test", lastName: "sss", documentType: "sss", documentId: "98789") {
    personId
    firstName
    lastName
    createdDate
    modifiedDate
    address(addressId: 1, street1: "street1", street2: "street2", street3: "street1", zip:"ss", city: "OR", state: "s", country:"ssss") {
      addressId
      city
      country
      state
    }

    contactInfo(contactInfo: [{contactId: 1, contact: "mss", contactType: "mail"}]) {
      contactId
      contact
      contactType
    }
  }
}
-}
