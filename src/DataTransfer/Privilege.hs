{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DataTransfer.Privilege where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data Privilege = Privilege {  privilegeId :: Int
                            , privilegeName :: Text
                            , description :: Maybe Text
                            , active :: Bool
                           } deriving (Generic, Show)

instance ToJSON Privilege where
instance FromJSON Privilege

-- PRIVILEGES GETTERS
getPrivilegeId Privilege {..} = privilegeId
getPrivilegeName Privilege {..} = privilegeName
getDescription Privilege {..} = description
getActive Privilege {..} = active