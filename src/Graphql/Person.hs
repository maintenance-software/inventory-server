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

module Graphql.Person (
                  Persons
                , Person
                , resolvePerson
                , PersonMut
                , PersonArg
                , resolveSavePerson_
                -- user exports
                , Users
                , User
                , UserMut
                , UserArg
                , resolveUser
                ) where

import Import
import GHC.Generics
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..), lift, Res, MutRes)
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Crypto.BCrypt
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Prelude as P
import qualified Data.Set as S
import Graphql.Utils
import Graphql.Role
import Graphql.Privilege
import Data.Time

data Person = Person { personId :: Int
                     , firstName :: Text
                     , lastName :: Text
                     , documentType :: Text
                     , documentId :: Text
                     , createdDate :: Text
                     , modifiedDate :: Maybe Text
                     , account :: DummyArg -> Res () Handler (Maybe User)
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
resolvePerson = Persons {  person = getPersonByIdResolver, list = listPersonResolver}

getPersonByIdResolver :: GetEntityByIdArg -> Res e Handler Person
getPersonByIdResolver GetEntityByIdArg {..} = lift $ do
                                      let personEntityId = (toSqlKey $ fromIntegral $ entityId)::Person_Id
                                      person <- runDB $ getJustEntity personEntityId
                                      return $ toPersonQL person

getPersonUserByIdResolver :: Person_Id -> DummyArg -> Res e Handler (Maybe User)
getPersonUserByIdResolver  personId _ = lift $ do
                                      userMaybe <- runDB $ selectFirst [User_PersonId ==. personId] []
                                      let user = case userMaybe of
                                                  Nothing -> Nothing
                                                  Just a -> Just $ toUserQL a
                                      return user

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

listPersonResolver :: ListArgs -> Res e Handler [Person]
listPersonResolver ListArgs{..} = lift $ do
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
                                             , account = getPersonUserByIdResolver personId
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
                           , account :: UserArg -> MutRes () Handler UserMut
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
                                                 , account = resolveSaveUser personId
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

data User = User { userId :: Int
                 , username :: Text
                 , email :: Text
                 , password :: Text
                 , active :: Bool
                 , createdDate :: Text
                 , modifiedDate :: Maybe Text
                 , person :: DummyArg -> Res () Handler Person
                 , privileges :: DummyArg -> Res () Handler [Privilege]
                 , roles :: DummyArg -> Res () Handler [Role]
                 } deriving (Generic, GQLType)

data Users m = Users { user :: GetEntityByIdArg -> m User
                     , list :: ListArgs -> m [User]
                     } deriving (Generic, GQLType)

-- Query Resolvers
resolveUser :: Users (Res () Handler)
resolveUser = Users {  user = getUserByIdResolver, list = listUserResolver}

getUserByIdResolver :: GetEntityByIdArg -> Res e Handler User
getUserByIdResolver GetEntityByIdArg {..} = lift $ do
                                      let userEntityId = (toSqlKey $ fromIntegral $ entityId)::User_Id
                                      user <- runDB $ getJustEntity userEntityId
                                      return $ toUserQL user

getUserPersonByIdResolver :: Person_Id -> DummyArg -> Res e Handler Person
getUserPersonByIdResolver personId _ = lift $ do
                                      person <- runDB $ getJustEntity personId
                                      return $ toPersonQL person

listUserResolver :: ListArgs -> Res e Handler [User]
listUserResolver ListArgs{..} = lift $ do
                                users <- runDB $ selectList [] [Asc User_Id, LimitTo size, OffsetBy $ (page - 1) * size]
                                return $ P.map (\p -> toUserQL p) users
                         where
                          (page, size) = case pageable of
                                          Just (Pageable x y) -> (x, y)
                                          Nothing -> (1, 10)

toUserQL :: Entity User_ -> User
toUserQL (Entity userId user) = User { userId = fromIntegral $ fromSqlKey userId
                                     , username = user_Username
                                     , email = user_Email
                                     , password = "********"
                                     , active = user_Active
                                     , createdDate = fromString $ show user_CreatedDate
                                     , modifiedDate = md
                                     , person = getUserPersonByIdResolver user_PersonId
--                                      , privileges = privilegeResolver
--                                      , roles = roleResolver
                                     }
                                 where
                                  User_ {..} = user
                                  md = case user_ModifiedDate of
                                        Just d -> Just $ fromString $ show d
                                        Nothing -> Nothing


-- User Graphql Arguments
data UserArg = UserArg { userId :: Int
                       , username :: Text
                       , email :: Text
                       , password :: Text
                       , active :: Bool
                       } deriving (Generic)

data UserMut = UserMut { userId :: Int
                       , username :: Text
                       , email :: Text
                       , password :: Text
                       , active :: Bool
                       , createdDate :: Text
                       , modifiedDate :: Maybe Text
--                        , privileges :: EntityIdsArg -> MutRes () Handler [Privilege]
--                        , roles :: EntityIdsArg -> MutRes () Handler [Role]
                       } deriving (Generic, GQLType)

resolveSaveUser :: Person_Id -> UserArg -> MutRes e Handler UserMut
resolveSaveUser personId arg = lift $ do
                                userId <- createOrUpdateUser personId arg
                                user <- runDB $ getJustEntity userId
                                let Entity _ User_ {..} = user
                                let md = case user_ModifiedDate of
                                          Just d -> Just $ fromString $ show d
                                          Nothing -> Nothing
                                return UserMut { userId = fromIntegral $ fromSqlKey userId
                                               , username = user_Username
                                               , email = user_Email
                                               , password = "********"
                                               , active = user_Active
                                               , createdDate = fromString $ show user_CreatedDate
                                               , modifiedDate = md
--                                                , privileges = resolveSaveRoles userId
--                                                , roles = resolveSaveRoles userId
                                               }

createOrUpdateUser :: Person_Id -> UserArg -> Handler User_Id
createOrUpdateUser personId userArg = do
                               let UserArg{..} = userArg
                               now <- liftIO getCurrentTime
                               userEntityId <- if userId > 0 then
                                            do
                                              let userKey = (toSqlKey $ fromIntegral userId)::User_Id
                                              _ <- runDB $ update userKey [ User_Username =. username
                                                                          , User_Email =. email
                                                                          , User_Active =. active
                                                                          , User_ModifiedDate =. Just now
                                                                          ]
                                              return userKey
                                            else
                                              do
--                                                 let personId = (toSqlKey $ fromIntegral 0)::Person_Id
                                                p <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ B.pack $ T.unpack password
                                                let passwordEncrypted = case p of
                                                                          Nothing -> ""
                                                                          Just b -> T.pack $ B.unpack b
                                                userKey <- runDB $ insert (fromUserQL personId userArg now Nothing passwordEncrypted)
                                                return userKey
                               return userEntityId

fromUserQL :: Person_Id -> UserArg -> UTCTime -> Maybe UTCTime -> Text -> User_
fromUserQL personId UserArg {..} cd md pass= User_ username email pass active personId cd md
