{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DataTransfer.User where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import DataTransfer.Role (Role)
import DataTransfer.Privilege (Privilege)

data User = User {  userId :: Int
                  , username :: Text
                  , email :: Text
                  , password :: Text
                  , active :: Bool
                  , roles :: [Role]
                  , privileges :: [Privilege]
                 } deriving (Generic, Show)

instance ToJSON User where
instance FromJSON User

-- USER GETTERS
getUserId User {..} = userId
getUsername User {..} = username
getEmail User {..} = email
getPassword User {..} = password
getActive User {..} = active
getRoles User {..} = roles
getPrivileges User {..} = privileges