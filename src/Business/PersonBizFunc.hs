{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards #-}

module Business.PersonBizFunc where

import qualified Data.Text as T
import qualified DataTransfer.Person as DT
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Prelude as P
import Database.Persist.Sql (rawSql)
import Text.RawString.QQ
import qualified DataTransfer.User as U
import Business.UserBizFunc (buildUserResponse, createOrUpdateUserBizFunc)
import Import


personSql :: Text
personSql = [r|
  SELECT ??, ??
  FROM t_person INNER JOIN t_user
  ON t_person.user_id = t_user.user_id
|]

-- GET PERSON BY ID
getPersonByIdBizFunc :: Person_Id -> Handler DT.Person
getPersonByIdBizFunc personId = do
                                  person <- runDB $ getJustEntity personId
                                  address <- runDB $ selectFirst [Address_PersonId ==. personId] []
                                  contacts <- runDB $ selectList [ContactInfo_PersonId ==. personId] []
                                  let userId = userId' person
                                  user <- runDB $ getEntity userId
                                  let response = buildPersonResponse person user address contacts
                                  return response
                             where
                                userId' (Entity _ (Person_ _ _ _ _ Nothing)) = (toSqlKey $ fromIntegral 0)::User_Id
                                userId' (Entity _ (Person_ _ _ _ _ (Just userId))) = userId

-- DELETE PERSON BY ID
deletePersonBizFunc :: Person_Id -> Handler Bool
deletePersonBizFunc personId = do
                                _ <- runDB $ delete personId
                                return True

-- LIST PERSONS ENDPOINT
listPersonsBizFunc :: Handler [Entity Person_]
listPersonsBizFunc = do
                      persons <- runDB $ selectList [Person_UserId !=. Nothing] [Asc Person_Id]
                      return persons

-- LIST PERSONS ENDPOINT
listPersonsUsersBizFunc :: Handler [DT.Person]
listPersonsUsersBizFunc = do
                            persons <- runDB $ rawSql "select ??, ?? from t_person inner join t_user on (t_person.user_id = t_user.user_id)" []
                            return (toPersonEntity persons)

toPersonEntity :: [(Entity Person_, Entity User_)] -> [DT.Person]
toPersonEntity [] = []
toPersonEntity ((personEntity, userEntity): xs) = itemResponse: toPersonEntity xs
                                              where
                                                itemResponse = buildPersonResponse personEntity (Just userEntity) Nothing []

-- CREATE OR UPDATE PERSON
createOrUpdatePersonBizFunc :: DT.Person -> Handler DT.Person
createOrUpdatePersonBizFunc person = do
                                       let personId' = DT.getPersonId person
                                       personId <- if personId' > 0 then
                                                    do
                                                      let personKey = (toSqlKey $ fromIntegral personId')::Person_Id
                                                      _ <- runDB $ update personKey [  Person_FirstName =. DT.getFirstName person
                                                                                     , Person_LastName =. DT.getLastName person
                                                                                     , Person_DocumentType =. DT.getDocumentType person
                                                                                     , Person_DocumentId =. DT.getDocumentId person
                                                                                    ]
                                                      return personKey
                                                    else
                                                      do
                                                        personKey <- runDB $ insert (fromPersonDT person)
                                                        return personKey
                                       addressId <- case DT.getAddress person of
                                                    Nothing -> do return ((toSqlKey $ fromIntegral 0)::Address_Id)
                                                    Just c -> do
                                                              let key = DT.getAddressId c
                                                              a <- if key > 0 then
                                                                      do
                                                                        let addressId' = (toSqlKey $ fromIntegral key)::Address_Id
                                                                        _ <- runDB $ update addressId' [  Address_Street1 =. DT.getStreet1 c
                                                                                                        , Address_Street2 =. DT.getStreet2 c
                                                                                                        , Address_Street3 =. DT.getStreet3 c
                                                                                                        , Address_Zip =. DT.getZip c
                                                                                                        , Address_City =. DT.getCity c
                                                                                                        , Address_State =. DT.getState c
                                                                                                        , Address_Country =. DT.getCountry c
                                                                                                       ]
                                                                        return addressId'
                                                                   else
                                                                      do
                                                                        addressId' <- runDB $ insert (fromAddressDT personId c)
                                                                        return addressId'
                                                              return a
                                       let c1 = filter (\c -> (DT.getContactId c) <= 0)  $ DT.getContactInfo person
                                       let c2 = filter (\c -> (DT.getContactId c) > 0)  $ DT.getContactInfo person
                                       contactIds <- runDB $ insertMany  $ [fromContactDT personId c | c <- c1]
                                       _ <- updateContact c2
                                       _ <- case (DT.getAccount person) of
                                              Nothing -> do return Nothing
                                              Just user -> do
                                                            user' <- createOrUpdateUserBizFunc user
                                                            let userId = (toSqlKey $ fromIntegral (U.getUserId user'))::User_Id
                                                            _ <- runDB $ update personId [ Person_UserId =. Just userId]
                                                            return Nothing
                                       response <- getPersonByIdBizFunc personId
                                       return response

updateContact [] = return ()
updateContact (x:xs)= do
                        let key = DT.getContactId x
                        let contactId = (toSqlKey $ fromIntegral key)::ContactInfo_Id
                        _ <- runDB $ update contactId [ ContactInfo_ContactType =. DT.getContactType x , ContactInfo_Contact =. DT.getContact x ]
                        _ <- updateContact xs
                        return ()

buildPersonResponse:: Entity Person_ -> Maybe (Entity User_) -> Maybe (Entity Address_) -> [Entity ContactInfo_] -> DT.Person
buildPersonResponse (Entity personId person) userEntity address contacts = DT.Person {  personId = fromIntegral $ fromSqlKey personId
                                                                                      , firstName = a
                                                                                      , lastName = b
                                                                                      , documentType = c
                                                                                      , documentId = d
                                                                                      , address = address'
                                                                                      , contactInfo = P.map toContactDT contacts
                                                                                      , account = user'
                                                                                     }
                                                                        where
                                                                          Person_ a b c d _ = person
                                                                          address' = case address of
                                                                                     Nothing -> Nothing
                                                                                     Just a -> Just $ (toAddressDT a)
                                                                          user' = case userEntity of
                                                                                  Nothing -> Nothing
                                                                                  Just a -> Just $ buildUserResponse a

-- street1 street2 street3 zip city state country

fromPersonDT :: DT.Person -> Person_
fromPersonDT DT.Person {..} = Person_ firstName lastName documentType documentId Nothing

fromContactDT :: Person_Id -> DT.Contact -> ContactInfo_
fromContactDT personId DT.Contact {..} = ContactInfo_ contactType contact personId

fromAddressDT :: Person_Id -> DT.Address -> Address_
fromAddressDT personId DT.Address {..} = Address_ street1 street2 street3 zip city state country personId

toContactDT :: Entity ContactInfo_ -> DT.Contact
toContactDT (Entity contactId (ContactInfo_ a b _)) = DT.Contact (fromIntegral $ fromSqlKey contactId) a b

toAddressDT :: Entity Address_ -> DT.Address
toAddressDT (Entity addressId (Address_ a b c d e f g _)) = DT.Address (fromIntegral $ fromSqlKey addressId) a b c d e f g
