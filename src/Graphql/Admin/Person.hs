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

module Graphql.Admin.Person (
                  Persons
                , Person
                , personResolver
                , PersonArg(..)
                , createUpdatePersonResolver
                , Users
                , User
                , Address
                , ContactInfo
                , AddressArg
                , ContactInfoArg
                , createOrUpdatePerson
                , createOrUpdateAddress
                , createOrUpdateContactInfo
                , addressResolver
                , contactInfoResolver
                , getPersonByIdResolver_
                ) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Graphql.Admin.Role
import Graphql.Admin.Privilege
import Data.Time
import Enums
import Graphql.Admin.DataTypes

-- Query Resolvers
--personResolver :: () -> Res e Handler Persons
personResolver _ = pure Persons { person = getPersonByIdResolver
                                , page = listPagedPersonResolver
                                , createUpdatePerson = createUpdatePersonResolver
                                }

--getPersonByIdResolver :: GetEntityByIdArg -> Res e Handler (Person Res)
getPersonByIdResolver GetEntityByIdArg {..} = lift $ do
                                      let personEntityId = (toSqlKey $ fromIntegral $ entityId)::Person_Id
                                      person <- runDB $ getJustEntity personEntityId
                                      return $ toPersonQL person

--getPersonByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Person_Id -> () -> o () Handler (Person o)
getPersonByIdResolver_ personId = lift $ do
                                      person <- runDB $ getJustEntity personId
                                      return $ toPersonQL person

--addressResolver :: Person_Id -> PersonAddressArg -> Res e Handler (Maybe Address)
addressResolver personId _ = lift $ do
                    addressMaybe <- runDB $ selectFirst [Address_PersonId ==. personId] []
                    let address = case addressMaybe of
                                    Nothing -> Nothing
                                    Just a -> Just $ toAddressQL a
                    return address

--contactInfoResolver :: Person_Id -> PersonContactInfoArg -> Res e Handler [ContactInfo]
contactInfoResolver personId _ = lift $ do
                                      contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                                      return $ P.map toContactQL contacts

--listPagedPersonResolver :: PageArg -> Res e Handler (Page (Person Res))
listPagedPersonResolver PageArg{..} = lift $ do
                                countItems <- runDB $ count ([] :: [Filter Person_])
                                persons <- runDB $ selectList [] [Asc Person_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                                let personsQL = P.map (\p -> toPersonQL p) persons
                                return Page { totalCount = countItems
                                            , content = personsQL
                                            , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
                                                                  , hasPreview = pageIndex' * pageSize' > 0
                                                                  , pageSize = pageSize'
                                                                  , pageIndex = pageIndex'
                                            }
                                }
                         where
                          pageIndex' = case pageIndex of
                                        Just  x  -> x
                                        Nothing -> 0
                          pageSize' = case pageSize of
                                          Just y -> y
                                          Nothing -> 10

-- Person Mutation Resolvers
--createUpdatePersonResolver :: PersonArg -> MutRes e Handler (Person MutRes)
createUpdatePersonResolver arg = lift $ do
                                personId <- createOrUpdatePerson arg
                                person <- runDB $ getJustEntity personId
                                return $ toPersonQL person

createOrUpdatePerson :: PersonArg -> Handler Person_Id
createOrUpdatePerson personArg = do
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
                               _ <- case address of
                                      Nothing -> return ()
                                      Just addressArg ->  do
                                                          _ <- createOrUpdateAddress personEntityId addressArg
                                                          return ()
                               _ <- createOrUpdateContactInfo personEntityId contactInfo
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

--toPersonQL :: Entity Person_ -> (Person Res)
toPersonQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Person_ -> Person o
toPersonQL (Entity personId person) = Person { personId = fromIntegral $ fromSqlKey personId
                                             , firstName = person_FirstName
                                             , lastName = person_LastName
                                             , documentType = person_DocumentType
                                             , documentId = person_DocumentId
                                             , createdDate = fromString $ show person_CreatedDate
                                             , modifiedDate = md
                                             , address = addressResolver personId
                                             , contactInfo = contactInfoResolver personId
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
  savePerson(personId:0, firstName: "test", lastName: "sss", documentType: "sss", documentId: "0") {
    personId
    firstName
    lastName
    createdDate
    modifiedDate
    address(addressId: 0, street1: "street1", street2: "street2", street3: "street1", zip:"ss", city: "OR", state: "s", country:"ssss") {
      addressId
      city
      country
      state
    }

    contactInfo(contactInfo: [{contactId: 0, contact: "mss", contactType: "mail"}]) {
      contactId
      contact
      contactType
    }
  }
}
-}
