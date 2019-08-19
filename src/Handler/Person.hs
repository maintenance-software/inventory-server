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

module Handler.Person where

import qualified Data.Text as T
import qualified DataTransfer.Person as DT
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Prelude as P
import Import


-- GET PERSON BY ID
getPersonIdR :: PersonId -> Handler Value
getPersonIdR personId = do
                        person <- runDB $ getJustEntity personId
                        address <- runDB $ selectFirst [AddressPersonId ==. personId] []
                        contacts <- runDB $ selectList [ContactInfoPersonId ==. personId] []
                        let response = buildPersonResponse person address contacts
                        returnJson response

-- CREATE DUMMY PERSON
postPersonR :: Handler Value
postPersonR = do
               person <- (requireCheckJsonBody :: Handler DT.Person)
               newPerson <- runDB $ insertEntity (fromPersonDT person)
               returnJson newPerson

putPersonR :: Handler Value
putPersonR = do
               person <- (requireCheckJsonBody :: Handler DT.Person)
               personId <- case DT.getPersonId person of
                            Nothing -> do
                                        personKey <- runDB $ insert (fromPersonDT person)
                                        return personKey
                            Just key -> do
                                         let personKey = (toSqlKey $ fromIntegral key)::PersonId
                                         _ <- runDB $ update personKey [  PersonFirstName =. DT.getFirstName person
                                                                        , PersonLastName =. DT.getLastName person
                                                                        , PersonDocumentType =. DT.getDocumentType person
                                                                        , PersonDocumentId =. DT.getDocumentId person
                                                                       ]
                                         return personKey
               addressId <- case DT.getAddress person of
                            Nothing -> do return ((toSqlKey $ fromIntegral 0)::AddressId)
                            Just c -> do
                                      a <- case DT.getAddressId c of
                                            Nothing -> do
                                                        addressId' <- runDB $ insert (fromAddressDT personId c)
                                                        return addressId'
                                            Just key -> do
                                                        let addressId' = (toSqlKey $ fromIntegral key)::AddressId
                                                        _ <- runDB $ update addressId' [  AddressStreet1 =. DT.getStreet1 c
                                                                                        , AddressStreet2 =. DT.getStreet2 c
                                                                                        , AddressStreet3 =. DT.getStreet3 c
                                                                                        , AddressZip =. DT.getZip c
                                                                                        , AddressCity =. DT.getCity c
                                                                                        , AddressState =. DT.getState c
                                                                                        , AddressCountry =. DT.getCountry c
                                                                                       ]
                                                        return addressId'
                                      return a
               let c1 = filter (\c -> (DT.getContactId c) == Nothing)  $ DT.getContactInfo person
               let c2 = filter (\c -> (DT.getContactId c) /= Nothing)  $ DT.getContactInfo person
               contactIds <- runDB $ insertMany  $ [fromContactDT personId c | c <- c1]
               _ <- updateContact c2
               response <- getPersonIdR personId
               returnJson response

updateContact [] = return ()
updateContact (x:xs)= do
                        let Just key = DT.getContactId x
                        let contactId = (toSqlKey $ fromIntegral key)::ContactInfoId
                        _ <- runDB $ update contactId [  ContactInfoType =. DT.getContactType x , ContactInfoContact =. DT.getContact x]
                        _ <- updateContact xs
                        return ()

buildPersonResponse:: Entity Person -> Maybe (Entity Address) -> [Entity ContactInfo] -> DT.Person
buildPersonResponse (Entity personId person) address contacts = DT.Person {  personId = Just $ fromIntegral $ fromSqlKey personId
                                                                           , firstName = a
                                                                           , lastName = b
                                                                           , documentType = c
                                                                           , documentId = d
                                                                           , address = address'
                                                                           , contactInfo = P.map toContactDT contacts
                                                                          }
                                                              where
                                                                Person a b c d _ = person
                                                                address' = case address of
                                                                           Nothing -> Nothing
                                                                           Just a -> Just $ (toAddressDT a)



-- street1 street2 street3 zip city state country

fromPersonDT :: DT.Person -> Person
fromPersonDT DT.Person {..} = Person firstName lastName documentType documentId Nothing

fromContactDT :: PersonId -> DT.Contact -> ContactInfo
fromContactDT personId DT.Contact {..} = ContactInfo contactType contact personId

fromAddressDT :: PersonId -> DT.Address -> Address
fromAddressDT personId DT.Address {..} = Address street1 street2 street3 zip city state country personId

toContactDT :: Entity ContactInfo -> DT.Contact
toContactDT (Entity contactId (ContactInfo a b _)) = DT.Contact (Just $ fromIntegral $ fromSqlKey contactId) a b

toAddressDT :: Entity Address -> DT.Address
toAddressDT (Entity addressId (Address a b c d e f g _)) = DT.Address (Just $ fromIntegral $ fromSqlKey addressId) a b c d e f g

-- postPersonR :: Handler Value
-- postPersonR = do
--                person <- (requireJsonBody :: Handler DT.Person)
--                let (DT.Person personId _ _ _ _ _) = person
--                newUser <- runDB $ get ((toSqlKey $ fromIntegral personId)::UserId)
--                returnJson newUser