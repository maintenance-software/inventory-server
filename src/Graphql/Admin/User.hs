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

module Graphql.Admin.User (
                  userResolver
                , getUserByIdResolver
                , getUserByIdResolver_
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
import Graphql.Admin.DataTypes
import Graphql.Admin.Role
import Graphql.Admin.Privilege
import Graphql.Admin.Person (createOrUpdatePerson, getPersonByIdResolver_)
import Data.Time
import Enums


-- Query Resolvers
--userResolver :: () -> Res e Handler Users
userResolver _ = pure Users { user = getUserByIdResolver
--                             , page = listUserResolver
                             , createUpdateUser = createUpdateUserResolver 
                             , resetPassword = resetPasswordResolver
                             , changePassword = changePasswordResolver
                             , updatePassword = updatePasswordResolver
                             }

--getUserByIdResolver :: EntityIdArg -> Res e Handler (User Res)
getUserByIdResolver EntityIdArg {..} = lift $ do
                                      let userEntityId = (toSqlKey $ fromIntegral $ entityId)::User_Id
                                      user <- runDB $ getJustEntity userEntityId
                                      return $ toUserQL user

getUserByIdResolver_ userId _ = lift $ do
                                      user <- runDB $ getJustEntity userId
                                      return $ toUserQL user

--resetPasswordResolver :: EntityIdArg -> Res e Handler Text
resetPasswordResolver EntityIdArg {..} = lift $ do
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
--getUserPersonByIdResolver personId _ = lift $ do
--                                      person <- runDB $ getJustEntity personId
--                                      return $ toPersonQL person

--listUserResolver :: PageArg -> Res e Handler [User Res]
--listUserResolver PageArg{..} = lift $ do
--                                users <- runDB $ selectList [] [Asc User_Id, LimitTo pageSize', OffsetBy $ pageIndex' * pageSize']
--                                return $ P.map (\p -> toUserQL p) users
--                         where
--                          pageIndex' = case pageIndex of
--                                        Just  x  -> x
--                                        Nothing -> 0
--                          pageSize' = case pageSize of
--                                          Just y -> y
--                                          Nothing -> 10

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

-- Mutation Resolvers
--createUpdateUserResolver :: Person_Id -> PersonUserArg -> MutRes e Handler (Maybe (User MutRes))
createUpdateUserResolver userArg = lift $ do
                                userId <- createOrUpdateUser userArg
                                user <- runDB $ getJustEntity userId
                                return $ toUserQL user

--createUpdateUserResolverRole :: User_Id -> UserRoleArg -> MutRes e Handler [Role MutRes]
--createUpdateUserResolverRole userId (UserRoleArg (Just entityIds)) = lift $ do
--                                          let entityRoleIds = P.map (\ x -> (toSqlKey $ fromIntegral $ x)::Role_Id) entityIds
--                                          () <- createUpdateUserRole userId entityRoleIds
--                                          userRoles <- runDB $ selectList ([UserRole_UserId ==. userId] :: [Filter UserRole_]) []
--                                          let roleIds = P.map (\(Entity _ (UserRole_ _ roleId)) -> roleId) userRoles
--                                          roles <- runDB $ selectList ([Role_Id <-. roleIds] :: [Filter Role_]) []
--                                          return $ P.map toRoleQL roles

--createUpdateUserResolverPrivilege :: User_Id -> UserPrivilegeArg -> MutRes e Handler [Privilege]
--createUpdateUserResolverPrivilege userId (UserPrivilegeArg (Just entityIds)) = lift $ do
--                                          let entityPrivilegeIds = P.map (\ x -> (toSqlKey $ fromIntegral $ x)::Privilege_Id) entityIds
--                                          () <- createOrUpdateUserPrivilege userId entityPrivilegeIds
--                                          userPrivileges <- runDB $ selectList ([UserPrivilege_UserId ==. userId] :: [Filter UserPrivilege_]) []
--                                          let privilegeIds = P.map (\(Entity _ (UserPrivilege_ _ privilegeId)) -> privilegeId) userPrivileges
--                                          privileges <- runDB $ selectList ([Privilege_Id <-. privilegeIds] :: [Filter Privilege_]) []
--                                          return $ P.map toPrivilegeQL privileges

--createOrUpdateUser :: Person_Id -> UserArg -> Handler User_Id
createOrUpdateUser userArg = do
                               let UserArg{..} = userArg
                               now <- liftIO getCurrentTime
                               personId <- createOrUpdatePerson person
                               userEntityId <- if userId > 0 then
                                            do
                                              let userKey = (toSqlKey $ fromIntegral userId)::User_Id
                                              _ <- runDB $ update userKey [ User_Username =. username
                                                                          , User_Email =. email
                                                                          , User_Status =. readEntityStatus status
                                                                          , User_Locale =. readLocale locale
                                                                          , User_Expiration =. expiration
                                                                          , User_ModifiedDate =. Just now
                                                                          ]
                                              return userKey
                                            else
                                              do
                                                userKey <- runDB $ insert (fromUserQL personId userArg now Nothing "$" True)
                                                return userKey
                               _ <- createUpdateUserRole userEntityId $ P.map (\ rId -> (toSqlKey $ fromIntegral rId)::Role_Id) roleIds
                               _ <- createOrUpdateUserPrivilege userEntityId $ P.map (\ pId -> (toSqlKey $ fromIntegral pId)::Privilege_Id) privilegeIds
                               return userEntityId

--createUpdateUserRole :: User_Id -> [Role_Id] -> Handler ()
createUpdateUserRole userId requestRoleIds = do
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


-- toUserQL :: Entity User_ -> User Res
toUserQL (Entity userId user) = User { userId = fromIntegral $ fromSqlKey userId
                                     , username = user_Username
                                     , email = user_Email
                                     , status = T.pack $ show user_Status
                                     , locale = T.pack $ show user_Locale
                                     , expiration = user_Expiration
                                     , createdDate = fromString $ show user_CreatedDate
                                     , modifiedDate = md
                                     , person = getPersonByIdResolver_ user_PersonId
                                     , privileges = userPrivilegeResolver userId
                                     , roles = userRoleResolver userId
                                     }
                                 where
                                  User_ {..} = user
                                  md = case user_ModifiedDate of
                                        Just d -> Just $ fromString $ show d
                                        Nothing -> Nothing


fromUserQL :: Person_Id -> UserArg -> UTCTime -> Maybe UTCTime -> Text -> Bool -> User_
fromUserQL personId UserArg {..} cd md pass newPasswordRequired = User_ { user_Username = username
                                                                        , user_Email =  email
                                                                        , user_Password = pass
                                                                        , user_Status = readEntityStatus status
                                                                        , user_Locale = readLocale locale
                                                                        , user_NewPasswordRequired = newPasswordRequired
                                                                        , user_Expiration = expiration
                                                                        , user_PersonId = personId
                                                                        , user_CreatedDate = cd
                                                                        , user_ModifiedDate = md
                                                                        }

