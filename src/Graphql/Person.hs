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
                , PersonArg(..)
                , resolveSavePerson
                , Users
                , User
                , resolveUser
                , Address
                , ContactInfo
                , AddressArg
                , ContactInfoArg
                , createOrUpdatePerson_
                , createOrUpdateAddress
                , createOrUpdateContactInfo
                , addressResolver_
                , contactInfoResolver_
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
import Graphql.Role
import Graphql.Privilege
import Data.Time
import Enums

data Person o = Person { personId :: Int
                       , firstName :: Text
                       , lastName :: Text
                       , documentType :: Text
                       , documentId :: Text
                       , createdDate :: Text
                       , modifiedDate :: Maybe Text
                       , address :: PersonAddressArg -> o () Handler (Maybe Address)
                       , contactInfo :: PersonContactInfoArg -> o () Handler [ContactInfo]
                       , account :: PersonUserArg -> o () Handler (Maybe (User o))
                       } deriving (Generic, GQLType)

data Persons = Persons { person :: GetEntityByIdArg -> Res () Handler (Person Res)
                       , page :: PageArg -> Res () Handler (Page (Person Res))
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
data PersonAddressArg = PersonAddressArg {address :: Maybe AddressArg} deriving (Generic, GQLType)
data PersonContactInfoArg = PersonContactInfoArg {contactInfo :: Maybe [ContactInfoArg]} deriving (Generic, GQLType)
data PersonUserArg = PersonUserArg {user :: Maybe UserArg} deriving (Generic, GQLType)
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

-- Query Resolvers
resolvePerson :: () -> Res e Handler Persons
resolvePerson _ = pure Persons {  person = getPersonByIdResolver, page = listPagedPersonResolver}

getPersonByIdResolver :: GetEntityByIdArg -> Res e Handler (Person Res)
getPersonByIdResolver GetEntityByIdArg {..} = lift $ do
                                      let personEntityId = (toSqlKey $ fromIntegral $ entityId)::Person_Id
                                      person <- runDB $ getJustEntity personEntityId
                                      return $ toPersonQL person

--getPersonByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Person_Id -> () -> o () Handler (Person o)
getPersonByIdResolver_ personId = lift $ do
                                      person <- runDB $ getJustEntity personId
                                      return $ toPersonQL person

--getPersonUserByIdResolver :: Person_Id -> PersonUserArg -> Res e Handler (Maybe (User Res))
getPersonUserByIdResolver  personId _ = lift $ do
                                      userMaybe <- runDB $ selectFirst [User_PersonId ==. personId] []
                                      let user = case userMaybe of
                                                  Nothing -> Nothing
                                                  Just a -> Just $ toUserQL a
                                      return user

--addressResolver_ :: Person_Id -> () -> Res e Handler (Maybe Address)
addressResolver_ personId _ = lift $ do
                    addressMaybe <- runDB $ selectFirst [Address_PersonId ==. personId] []
                    let address = case addressMaybe of
                                    Nothing -> Nothing
                                    Just a -> Just $ toAddressQL a
                    return address

--resolveAddress :: Person_Id -> PersonAddressArg -> Res e Handler (Maybe Address)
resolveAddress personId _ = lift $ do
                    addressMaybe <- runDB $ selectFirst [Address_PersonId ==. personId] []
                    let address = case addressMaybe of
                                    Nothing -> Nothing
                                    Just a -> Just $ toAddressQL a
                    return address

--resolveContactInfo :: Person_Id -> PersonContactInfoArg -> Res e Handler [ContactInfo]
resolveContactInfo personId _ = lift $ do
                                      contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                                      return $ P.map toContactQL contacts

--contactInfoResolver_ :: Person_Id -> () -> Res e Handler [ContactInfo]
contactInfoResolver_ personId _ = lift $ do
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

--toPersonQL :: Entity Person_ -> (Person Res)
toPersonQL :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Entity Person_ -> Person o
toPersonQL (Entity personId person) = Person { personId = fromIntegral $ fromSqlKey personId
                                             , firstName = person_FirstName
                                             , lastName = person_LastName
                                             , documentType = person_DocumentType
                                             , documentId = person_DocumentId
                                             , createdDate = fromString $ show person_CreatedDate
                                             , modifiedDate = md
                                             , address = resolveAddress personId
                                             , contactInfo = resolveContactInfo personId
                                             , account = getPersonUserByIdResolver personId
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

-- Person Mutation Resolvers
--resolveSavePerson :: PersonArg -> MutRes e Handler (Person MutRes)
resolveSavePerson arg = lift $ do
                                personId <- createOrUpdatePerson_ arg
                                person <- runDB $ getJustEntity personId
                                let Entity _ Person_ {..} = person
                                let md = case person_ModifiedDate of
                                          Just d -> Just $ fromString $ show d
                                          Nothing -> Nothing
                                return Person { personId = fromIntegral $ fromSqlKey personId
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

--resolveSaveAddress :: Person_Id -> PersonAddressArg -> MutRes e Handler (Maybe Address)
resolveSaveAddress personId PersonAddressArg {address} = lift $ do
                                          addressReponse <- case address of
                                                              Just c -> do
                                                                          addressId <- createOrUpdateAddress personId c
                                                                          addressEntity <- runDB $ getJustEntity addressId
                                                                          return $ Just $ toAddressQL addressEntity
                                                              Nothing -> return Nothing
                                          return addressReponse

--resolveSaveContactInfo :: Person_Id -> PersonContactInfoArg -> MutRes e Handler [ContactInfo]
resolveSaveContactInfo personId PersonContactInfoArg {..} = lift $ do
                                          () <- case contactInfo of
                                                  Just c -> createOrUpdateContactInfo personId c
                                                  Nothing -> return ()
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

data User o = User { userId :: Int
                   , username :: Text
                   , email :: Text
                   , status :: Text
                   , language :: Text
                   , expiration :: Bool
                   , newPasswordRequired :: Bool
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   , person :: () -> o () Handler (Person o)
                   , privileges :: UserPrivilegeArg -> o () Handler [Privilege]
                   , roles :: UserRoleArg -> o () Handler [Role o]
                   } deriving (Generic, GQLType)

data Users = Users { user :: GetEntityByIdArg -> Res () Handler (User Res)
                   , resetPassword :: GetEntityByIdArg -> Res () Handler Text
                   , changePassword :: ChangePasswordArg -> Res () Handler Bool
                   , updatePassword :: UpdatePasswordArg -> Res () Handler Bool
                   , list :: PageArg -> Res () Handler [User Res]
                   } deriving (Generic, GQLType)

data ChangePasswordArg = ChangePasswordArg { userId:: Int, password :: Text, newPassword :: Text} deriving (Generic)
data UpdatePasswordArg = UpdatePasswordArg { userId:: Int, password :: Text } deriving (Generic)
-- User Graphql Arguments
data UserArg = UserArg { userId :: Int
                       , username :: Text
                       , email :: Text
                       , status :: Text
                       , language :: Text
                       , expiration :: Bool
                       } deriving (Generic)

instance GQLType UserArg where
    type  KIND UserArg = INPUT_OBJECT
    description = const $ Just $ pack "The item that holds the user information"

data UserRoleArg = UserRoleArg {roleIds :: Maybe [Int]} deriving (Generic, GQLType)
data UserPrivilegeArg = UserPrivilegeArg { privilegeIds :: Maybe [Int]} deriving (Generic, GQLType)

-- Query Resolvers
--resolveUser :: () -> Res e Handler Users
resolveUser _ = pure Users { user = getUserByIdResolver
                           , list = listUserResolver
                           , resetPassword = resetPasswordResolver
                           , changePassword = changePasswordResolver
                           , updatePassword = updatePasswordResolver
                           }

--getUserByIdResolver :: GetEntityByIdArg -> Res e Handler (User Res)
getUserByIdResolver GetEntityByIdArg {..} = lift $ do
                                      let userEntityId = (toSqlKey $ fromIntegral $ entityId)::User_Id
                                      user <- runDB $ getJustEntity userEntityId
                                      return $ toUserQL user

--resetPasswordResolver :: GetEntityByIdArg -> Res e Handler Text
resetPasswordResolver GetEntityByIdArg {..} = lift $ do
                                      password <- liftIO $ genRandomAlphaNumString 8
                                      hashedPassword <- liftIO $ hashPassword 6 (encodeUtf8 $ T.pack password)
                                      let passwordEncrypted = T.pack $ B.unpack hashedPassword
                                      let userEntityId = (toSqlKey $ fromIntegral $ entityId)::User_Id
                                      _ <- runDB $ update userEntityId [ User_Password =. passwordEncrypted, User_NewPasswordRequired =. True ]
                                      return $ T.pack password

--changePasswordResolver :: ChangePasswordArg -> Res e Handler Bool
changePasswordResolver ChangePasswordArg {..} = lift $ do
                                      p <- liftIO $ hashPassword 6 (encodeUtf8 password)
                                      let hashedPassword = T.pack $ B.unpack p
                                      let userEntityId = (toSqlKey $ fromIntegral $ userId)::User_Id
                                      userEntity <- runDB $ getJustEntity userEntityId
                                      let Entity _ user = userEntity
                                      let User_ {..} = user
                                      result <- case user_Password == hashedPassword of
                                                True -> do
                                                          q <- liftIO $ hashPassword 6 (encodeUtf8 newPassword)
                                                          let hashedNewPassword = T.pack $ B.unpack q
                                                          () <- runDB $ update userEntityId [ User_Password =. hashedNewPassword, User_NewPasswordRequired =. False ]
                                                          return True
                                                False ->  return False
                                      return result

--updatePasswordResolver :: UpdatePasswordArg -> Res e Handler Bool
updatePasswordResolver UpdatePasswordArg {..} = lift $ do
                                      let userEntityId = (toSqlKey $ fromIntegral $ userId)::User_Id
                                      userEntity <- runDB $ getJustEntity userEntityId
                                      let Entity _ user = userEntity
                                      let User_ {..} = user
                                      result <- case user_NewPasswordRequired of
                                                True -> do
                                                          p <- liftIO $ hashPassword 6 (encodeUtf8 password)
                                                          let hashedPassword = T.pack $ B.unpack p
                                                          () <- runDB $ update userEntityId [ User_Password =. hashedPassword, User_NewPasswordRequired =. False ]
                                                          return True
                                                False ->  return False
                                      return result

--getUserPersonByIdResolver :: Person_Id -> () -> Res e Handler (Person Res)
getUserPersonByIdResolver personId _ = lift $ do
                                      person <- runDB $ getJustEntity personId
                                      return $ toPersonQL person

--listUserResolver :: PageArg -> Res e Handler [User Res]
listUserResolver PageArg{..} = lift $ do
                                users <- runDB $ selectList [] [Asc User_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
                                return $ P.map (\p -> toUserQL p) users
                         where
                          pageIndex' = case pageIndex of
                                        Just  x  -> x
                                        Nothing -> 0
                          pageSize' = case pageSize of
                                          Just y -> y
                                          Nothing -> 10

--userPrivilegeResolver :: User_Id -> UserPrivilegeArg -> Res e Handler [Privilege]
userPrivilegeResolver userId _ = lift $ do
                                      userPrivileges <- runDB $ selectList ([UserPrivilege_UserId ==. userId] :: [Filter UserPrivilege_]) []
                                      let privilegeIds = P.map (\(Entity _ (UserPrivilege_ _ privilegeId)) -> privilegeId) userPrivileges
                                      privileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
                                      return $ P.map toPrivilegeQL privileges

--userRoleResolver :: User_Id -> UserRoleArg -> Res e Handler [Role Res]
userRoleResolver userId _ = lift $ do
                                      userRoles <- runDB $ selectList ([UserRole_UserId ==. userId] :: [Filter UserRole_]) []
                                      let roleIds = P.map (\(Entity _ (UserRole_ _ roleId)) -> roleId) userRoles
                                      roles <- runDB $ selectList ([Role_Id <-. roleIds] :: [Filter Role_]) []
                                      return $ P.map toRoleQL roles

-- toUserQL :: Entity User_ -> User Res
toUserQL (Entity userId user) = User { userId = fromIntegral $ fromSqlKey userId
                                     , username = user_Username
                                     , email = user_Email
                                     , status = T.pack $ show user_Status
                                     , language = T.pack $ show user_Language
                                     , expiration = user_Expiration
                                     , createdDate = fromString $ show user_CreatedDate
                                     , modifiedDate = md
                                     , person = getUserPersonByIdResolver user_PersonId
                                     , privileges = userPrivilegeResolver userId
                                     , roles = userRoleResolver userId
                                     }
                                 where
                                  User_ {..} = user
                                  md = case user_ModifiedDate of
                                        Just d -> Just $ fromString $ show d
                                        Nothing -> Nothing

-- Mutation Resolvers
--resolveSaveUser :: Person_Id -> PersonUserArg -> MutRes e Handler (Maybe (User MutRes))
resolveSaveUser personId (PersonUserArg (Just arg) ) = lift $ do
                                userId <- createOrUpdateUser personId arg
                                user <- runDB $ getJustEntity userId
                                let Entity _ User_ {..} = user
                                let md = case user_ModifiedDate of
                                          Just d -> Just $ fromString $ show d
                                          Nothing -> Nothing
                                return $ Just User { userId = fromIntegral $ fromSqlKey userId
                                               , username = user_Username
                                               , email = user_Email
                                               , status = T.pack $ show user_Status
                                               , language = T.pack $ show user_Language
                                               , expiration = user_Expiration
                                               , createdDate = fromString $ show user_CreatedDate
                                               , modifiedDate = md
                                               , privileges = resolveSaveUserPrivilege userId
                                               , roles = resolveSaveUserRole userId
                                               }

--resolveSaveUserRole :: User_Id -> UserRoleArg -> MutRes e Handler [Role MutRes]
resolveSaveUserRole userId (UserRoleArg (Just entityIds)) = lift $ do
                                          let entityRoleIds = P.map (\ x -> (toSqlKey $ fromIntegral $ x)::Role_Id) entityIds
                                          () <- addUserRole userId entityRoleIds
                                          userRoles <- runDB $ selectList ([UserRole_UserId ==. userId] :: [Filter UserRole_]) []
                                          let roleIds = P.map (\(Entity _ (UserRole_ _ roleId)) -> roleId) userRoles
                                          roles <- runDB $ selectList ([Role_Id <-. roleIds] :: [Filter Role_]) []
                                          return $ P.map toRoleQL roles

--resolveSaveUserPrivilege :: User_Id -> UserPrivilegeArg -> MutRes e Handler [Privilege]
resolveSaveUserPrivilege userId (UserPrivilegeArg (Just entityIds)) = lift $ do
                                          let entityPrivilegeIds = P.map (\ x -> (toSqlKey $ fromIntegral $ x)::Privilege_Id) entityIds
                                          () <- createOrUpdateUserPrivilege userId entityPrivilegeIds
                                          userPrivileges <- runDB $ selectList ([UserPrivilege_UserId ==. userId] :: [Filter UserPrivilege_]) []
                                          let privilegeIds = P.map (\(Entity _ (UserPrivilege_ _ privilegeId)) -> privilegeId) userPrivileges
                                          privileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
                                          return $ P.map toPrivilegeQL privileges

--createOrUpdateUser :: Person_Id -> UserArg -> Handler User_Id
createOrUpdateUser personId userArg = do
                               let UserArg{..} = userArg
                               now <- liftIO getCurrentTime
                               userEntityId <- if userId > 0 then
                                            do
                                              let userKey = (toSqlKey $ fromIntegral userId)::User_Id
                                              _ <- runDB $ update userKey [ User_Username =. username
                                                                          , User_Email =. email
                                                                          , User_Status =. readEntityStatus status
                                                                          , User_Language =. readLocale language
                                                                          , User_Expiration =. expiration
                                                                          , User_ModifiedDate =. Just now
                                                                          ]
                                              return userKey
                                            else
                                              do
                                                userKey <- runDB $ insert (fromUserQL personId userArg now Nothing "$" True)
                                                return userKey
                               return userEntityId

--addUserRole :: User_Id -> [Role_Id] -> Handler ()
addUserRole userId requestRoleIds = do
                                     entityUserRoles <- runDB $ selectList ([UserRole_UserId ==. userId] :: [Filter UserRole_]) []
                                     let existingRoleIds = P.map (\(Entity _ (UserRole_ _ roleId)) -> roleId) entityUserRoles
                                     let removableIds = S.toList $ S.difference (S.fromList existingRoleIds) (S.fromList requestRoleIds)
                                     let newIds = S.toList $ S.difference (S.fromList requestRoleIds) (S.fromList existingRoleIds)
                                     _ <- runDB $ deleteWhere  ([UserRole_RoleId <-. removableIds] :: [Filter UserRole_])
                                     _ <- runDB $ insertMany $ P.map (\roleId -> (UserRole_ userId roleId)) newIds
                                     return ()

--createOrUpdateUserPrivilege :: User_Id -> [Privilege_Id] -> Handler ()
createOrUpdateUserPrivilege userId entityPrivilegeIds = do
                            entityUserPrivileges <- runDB $ selectList ([UserPrivilege_UserId ==. userId] :: [Filter UserPrivilege_]) []
                            let existingPrivilegeIds = P.map (\(Entity _ (UserPrivilege_ _ privilegeId)) -> privilegeId) entityUserPrivileges
                            let removableIds = S.toList $ S.difference (S.fromList existingPrivilegeIds) (S.fromList entityPrivilegeIds)
                            let newIds = S.toList $ S.difference (S.fromList entityPrivilegeIds) (S.fromList existingPrivilegeIds)
                            _ <- runDB $ deleteWhere  ([UserPrivilege_PrivilegeId <-. removableIds] :: [Filter UserPrivilege_])
                            _ <- runDB $ insertMany $ P.map (\privilegeId -> (UserPrivilege_ userId privilegeId)) newIds
                            return ()

fromUserQL :: Person_Id -> UserArg -> UTCTime -> Maybe UTCTime -> Text -> Bool -> User_
fromUserQL personId UserArg {..} cd md pass newPasswordRequired = User_ { user_Username = username
                                                                        , user_Email =  email
                                                                        , user_Password = pass
                                                                        , user_Status = readEntityStatus status
                                                                        , user_Language = readLocale language
                                                                        , user_NewPasswordRequired = newPasswordRequired
                                                                        , user_Expiration = expiration
                                                                        , user_PersonId = personId
                                                                        , user_CreatedDate = cd
                                                                        , user_ModifiedDate = md
                                                                        }

