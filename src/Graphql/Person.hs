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

data Person = Person { personId :: Int
                     , firstName :: Text
                     , lastName :: Text
                     , documentType :: Text
                     , documentId :: Text
                     , address :: Maybe Address
                     , contactInfo :: [ContactInfo]
                     } deriving (Generic, GQLType)

data ContactInfo = ContactInfo { contactId :: Int
                               , contact :: Text
                               , contactType :: Text
                               } deriving (Generic, GQLType)

data Address = Address {  addressId :: Int
                        , street1 :: Text
                        , street2 :: Text
                        , street3 :: Text
                        , zip :: Text
                        , city :: Text
                        , state :: Text
                        , country :: Text
                       } deriving (Generic, GQLType)

data Persons m = Persons { findById :: FindPersonByIdArgs -> m Person
                         , list :: ListArgs -> m [Person]
                         } deriving (Generic, GQLType)

data FindPersonByIdArgs = FindPersonByIdArgs { personId :: Int } deriving (Generic)


-- DB ACTIONS
dbFetchPersonById:: Person_Id -> Handler Person
dbFetchPersonById personId = do
                              person <- runDB $ getJustEntity personId
                              address <- runDB $ selectFirst [Address_PersonId ==. personId] []
                              contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                              let response = toPersonQL person address contacts
                              return response

dbFetchPersons:: ListArgs -> Handler [Person]
dbFetchPersons ListArgs {..} = do
                              persons <- runDB $ selectList [] [Asc Person_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                              return $ P.map (\p -> toPersonQL p Nothing []) persons
                         where
                          (page, size) = case pageable of
                                          Just (Pageable x y) -> (x, y)
                                          Nothing -> (1, 10)



-- Query Resolvers
findByIdResolver :: FindPersonByIdArgs -> Res e Handler Person
findByIdResolver FindPersonByIdArgs { personId } = lift $ dbFetchPersonById roleKey
                                              where
                                                roleKey = (toSqlKey $ fromIntegral $ personId)::Person_Id

listResolver :: ListArgs -> Res e Handler [Person]
listResolver listArgs = lift $ dbFetchPersons listArgs

resolvePerson :: Persons (Res () Handler)
resolvePerson = Persons {  findById = findByIdResolver, list = listResolver }

-- CONVERTERS
--     Id sql=role_id
--     key Text
--     name Text
--     description Text Maybe
--     active Bool
--     createdDate UTCTime
--     modifiedDate UTCTime
{--
fromRoleQL :: Person -> UTCTime -> Maybe UTCTime -> Role_
fromRoleQL (Person {..}) cd md = Role_ { role_Key = key
                                          , role_Name = name
                                          , role_Description = description
                                          , role_Active = active
                                          , role_CreatedDate = cd
                                          , role_ModifiedDate = md
                                          }

fromRoleArg :: RoleArg -> UTCTime -> Maybe UTCTime -> Role_
fromRoleArg (RoleArg {..}) cd md = Role_ { role_Key = key
                                         , role_Name = name
                                         , role_Description = description
                                         , role_Active = active
                                         , role_CreatedDate = cd
                                         , role_ModifiedDate = md
                                         }
-}

toPersonQL:: Entity Person_ -> Maybe (Entity Address_) -> [Entity ContactInfo_] -> Person
toPersonQL (Entity personId person) address contacts = Person { personId = fromIntegral $ fromSqlKey personId
                                                              , firstName = person_FirstName
                                                              , lastName = person_LastName
                                                              , documentType = person_DocumentType
                                                              , documentId = person_DocumentId
                                                              , address = address'
                                                              , contactInfo = P.map toContactQL contacts
                                                              }
                                                    where
                                                      Person_ {..} = person
                                                      address' = case address of
                                                                 Nothing -> Nothing
                                                                 Just a -> Just $ (toAddressQL a)

toContactQL :: Entity ContactInfo_ -> ContactInfo
toContactQL (Entity contactId contact) = ContactInfo { contactId = fromIntegral $ fromSqlKey contactId
                                                     , contact = contactInfo_Contact
                                                     , contactType = contactInfo_ContactType
                                                     }
                                    where
                                      ContactInfo_ {..} = contact

toAddressQL :: Entity Address_ -> Address
toAddressQL (Entity addressId address) = Address { addressId = fromIntegral $ fromSqlKey addressId
                                                         , street1 = address_Street1
                                                         , street2 = address_Street2
                                                         , street3 = address_Street3
                                                         , zip = address_Zip
                                                         , city = address_City
                                                         , state = address_State
                                                         , country = address_Country
                                                         }
                                             where
                                                Address_ {..} = address

fromPersonQL :: Person -> Person_
fromPersonQL Person {..} = Person_ firstName lastName documentType documentId

fromAddressQL :: Person_Id -> Address -> Address_
fromAddressQL personId Address {..} = Address_ street1 street2 street3 zip city state country personId

fromContactQL :: Person_Id -> ContactInfo -> ContactInfo_
fromContactQL personId ContactInfo {..} = ContactInfo_ contactType contact personId

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
                         , address :: AddressArg -> MutRes () Handler Address
                         , contactInfo :: ContactInfoArgWrapper -> MutRes () Handler [ContactInfo]
                         } deriving (Generic, GQLType)

resolveSavePerson_ :: PersonArg -> MutRes e Handler PersonMut
resolveSavePerson_ arg = lift $ do
                                personId <- createOrUpdatePerson_ arg
                                person <- runDB $ getJustEntity personId
                                let Entity _ Person_ {..} = person
                                return PersonMut { personId = fromIntegral $ fromSqlKey personId
                                                , firstName = person_FirstName
                                                , lastName = person_LastName
                                                , documentType = person_DocumentType
                                                , documentId = person_DocumentId
                                                , address = resolveSaveAddress personId
                                                , contactInfo = resolveSaveContactInfo personId
                                                }

resolveSaveAddress :: Person_Id -> AddressArg -> MutRes e Handler Address
resolveSaveAddress personId arg = lift $ do
                                          addressId <- createOrUpdateAddress personId arg
                                          address <- runDB $ getJustEntity addressId
                                          let Entity _ Address_ {..} = address
                                          return Address { addressId = fromIntegral $ fromSqlKey addressId
                                                         , street1 = address_Street1
                                                         , street2 = address_Street2
                                                         , street3 = address_Street3
                                                         , zip = address_Zip
                                                         , city = address_City
                                                         , state = address_State
                                                         , country = address_Country
                                                         }

resolveSaveContactInfo :: Person_Id -> ContactInfoArgWrapper -> MutRes e Handler [ContactInfo]
resolveSaveContactInfo personId ContactInfoArgWrapper {..} = lift $ do
                                          () <- createOrUpdateContactInfo personId contactInfo
                                          contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                                          return $ P.map toContactQL contacts

createOrUpdatePerson_ :: PersonArg -> Handler Person_Id
createOrUpdatePerson_ personArg = do
                               let PersonArg{..} = personArg
                               personEntityId <- if personId > 0 then
                                            do
                                              let personKey = (toSqlKey $ fromIntegral personId)::Person_Id
                                              _ <- runDB $ update personKey [  Person_FirstName =. firstName
                                                                             , Person_LastName =. lastName
                                                                             , Person_DocumentType =. documentType
                                                                             , Person_DocumentId =. documentId
                                                                            ]
                                              return personKey
                                            else
                                              do
                                                personKey <- runDB $ insert (fromPersonQL_ personArg)
                                                return personKey
                               return personEntityId

createOrUpdateAddress :: Person_Id -> AddressArg -> Handler Address_Id
createOrUpdateAddress personId address = do
                               let AddressArg {..} = address
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
                                                                                    ]
                                                     return addressId'
                                                  else
                                                   do
                                                     addressId' <- runDB $ insert (fromAddressQL_ personId address)
                                                     return addressId'
                               return addressEntityId

createOrUpdateContactInfo :: Person_Id -> [ContactInfoArg] -> Handler ()
createOrUpdateContactInfo personId  contactInfo = do
                               let c1 = P.filter (\ContactInfoArg {..} -> contactId <= 0)  $ contactInfo
                               let c2 = P.filter (\ContactInfoArg {..} -> contactId > 0)  $ contactInfo
                               contactIds <- runDB $ insertMany  $ [fromContactQL_ personId c | c <- c1]
                               _ <- updateContact_ c2
                               return ()

updateContact_ [] = return ()
updateContact_ (x:xs)= do
                        let ContactInfoArg {..} = x
                        let entityContactId = (toSqlKey $ fromIntegral contactId)::ContactInfo_Id
                        _ <- runDB $ update entityContactId [  ContactInfo_ContactType =. contactType, ContactInfo_Contact =. contact]
                        _ <- updateContact_ xs
                        return ()

fromPersonQL_ :: PersonArg -> Person_
fromPersonQL_ PersonArg {..} = Person_ firstName lastName documentType documentId

fromAddressQL_ :: Person_Id -> AddressArg -> Address_
fromAddressQL_ personId AddressArg {..} = Address_ street1 street2 street3 zip city state country personId

fromContactQL_ :: Person_Id -> ContactInfoArg -> ContactInfo_
fromContactQL_ personId ContactInfoArg {..} = ContactInfo_ contactType contact personId
