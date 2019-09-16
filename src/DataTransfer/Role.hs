{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DataTransfer.Role where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import DataTransfer.Privilege (Privilege)


data Role = Role {  roleId :: Int
                  , roleName :: Text
                  , description :: Text
                  , active :: Bool
                  , privileges :: [Privilege]
                 } deriving (Generic, Show)

instance ToJSON Role where
instance FromJSON Role

--ROLE GETTERS
getRoleId Role {..} = roleId
getRoleName Role {..} = roleName
getDescription Role {..} = description
getActive Role {..} = active
getPrivileges Role {..} = privileges