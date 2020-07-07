{-# OPTIONS_GHC -w #-}
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


module Graphql.Root (api, apiDoc) where

import           GHC.Generics
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     ()
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLType(..), Undefined(..), Res, MutRes, GQLRequest, GQLResponse)
import           Data.Morpheus.Document (toGraphQLDocument)
import           Import
import           Data.ByteString.Lazy.Internal (ByteString)
import           Graphql.Session
import           Graphql.Admin.Privilege
import           Graphql.Admin.Role
import           Graphql.Admin.DataTypes
import           Graphql.Admin.Person
import           Graphql.Admin.User
import           Graphql.Category
import           Graphql.Asset.Unit
--import           Graphql.Maintenance.SubTask.SubTaskKind
--import           Graphql.Maintenance.Task.TaskCategory
import           Graphql.Asset.Item.Resolvers
import           Graphql.Asset.Inventory.Resolvers
import           Graphql.Maintenance.Resolvers
import           Graphql.Maintenance.DataTypes
import           Graphql.Asset.Equipment.Resolvers
import           Graphql.Asset.InventoryItem.Resolvers
import           Graphql.Utils ()
import           Graphql.Asset.DataTypes
import           Graphql.DataTypes
import           Graphql.WorkOrder.Resolvers (workOrderResolver)
import           Graphql.WorkQueue.Resolvers (workQueueResolver)
import           Graphql.Asset.Human.Resolvers
import           Graphql.Asset.Human.DataTypes
--import           Graphql.Asset.Human.EmployeeJob
-- importGQLDocumentWithNamespace "schema.gql"

data QueryQL m = QueryQL { -- deity :: DeityArgs -> m Deity
                           session :: () -> Res () Handler Session
                         , privileges :: () -> m Privileges
                         , roles :: () -> m Roles
--                         , persons :: () -> m Persons
--                         , users :: () -> m Users
                         , persons :: () -> Res () Handler (Persons Res)
                         , users :: () -> Res () Handler (Users Res)
                         , categories :: CategoryFilter -> m [Category]
                         , units :: () -> m [Unit]
                         , inventories :: () -> Res () Handler (Inventories Res)
                         , items :: () -> Res () Handler (Items Res)
                         , equipments :: () -> Res () Handler (Equipments Res)
                         , employees :: () -> Res () Handler (Employees Res)
                         , inventoryItems :: () -> Res () Handler (InventoryItems Res)
                         , maintenances :: () -> Res () Handler (Maintenances Res)
                         , workQueues :: () -> Res () Handler (WorkQueues Res)
                         , workOrders :: () -> Res () Handler (WorkOrders Res)
                         } deriving (Generic, GQLType)

data Mutation m = Mutation { savePrivilege :: PrivilegeArg -> m Privilege
                           , saveRole :: RoleArg -> m (Role MutRes)
                           , persons :: () -> MutRes () Handler (Persons MutRes)
                           , users :: () -> MutRes () Handler (Users MutRes)
--                           , savePerson :: PersonArg -> m (Person MutRes)
                           , saveCategory :: CategoryArg -> m Category
                           , saveUnit :: UnitArg -> m Unit
--                           , saveTaskCategory :: TaskCategoryArg -> m TaskCategory
--                           , saveSubTaskKind :: SubTaskKindArg -> m SubTaskKind
--                           , saveEmployeeJob :: EmployeeJobArg -> m EmployeeJob
--                           , saveInventory :: InventoryArg -> m (Inventory MutRes)
--                           , saveItem :: ItemArg -> m (Item MutRes)
--                           , saveInventoryItem :: InventoryItemArg -> m (InventoryItem MutRes)
                           , inventoryItems :: () -> MutRes () Handler (InventoryItems MutRes)
                           , items :: () -> MutRes () Handler (Items MutRes)
                           , equipments :: () -> MutRes () Handler (Equipments MutRes)
                           , employees :: () -> MutRes () Handler (Employees MutRes)
                           , inventories :: () -> MutRes () Handler (Inventories MutRes)
                           , maintenances :: () -> MutRes () Handler (Maintenances MutRes)
                           , workQueues :: () -> MutRes () Handler (WorkQueues MutRes)
                           , workOrders :: () -> MutRes () Handler (WorkOrders MutRes)
                           } deriving (Generic, GQLType)

--data DeityArgs = DeityArgs { name :: Text, mythology :: Maybe Text } deriving (Generic)

-- | The query resolver
resolveQuery::QueryQL (Res () Handler)
resolveQuery = QueryQL { --deity = resolveDeity
                         session = getUserSessionResolver
                       , privileges = resolvePrivilege
                       , roles = resolveRole
                       , persons = personResolver
                       , users = userResolver
                       , categories = listCategoryResolver
                       , units = listUnitResolver
--                       , taskCategories = listTaskCategoryResolver
--                       , subTaskKinds = listSubTaskKindResolver
--                       , employeeJobs = listEmployeeJobResolver
                       , inventories = inventoryResolver
                       , maintenances = maintenanceResolver
                       , employees = employeeResolver
                       , items = itemResolver
                       , equipments = equipmentResolver
                       , inventoryItems = inventoryItemsResolver
                       , workQueues = workQueueResolver
                       , workOrders = workOrderResolver
                       }
-- | The mutation resolver
resolveMutation::Mutation (MutRes () Handler)
resolveMutation = Mutation { savePrivilege = resolveSavePrivilege
                           , saveRole =  resolveSaveRole
--                           , savePerson = resolveSavePerson
                           , persons = personResolver
                           , users = userResolver
                           , saveCategory = saveCategoryResolver
                           , saveUnit = saveUnitResolver
--                           , saveTaskCategory = saveTaskCategoryResolver
--                           , saveSubTaskKind = saveSubTaskKindResolver
--                           , saveEmployeeJob = saveEmployeeJobResolver
--                           , saveInventory = saveInventoryResolver
--                           , saveItem =  saveItemResolver
--                           , saveInventoryItem =  saveInventoryItemResolver
                           , inventories = inventoryResolver
                           , maintenances = maintenanceResolver
                           , employees = employeeResolver
                           , items = itemResolver
                           , equipments = equipmentResolver
                           , inventoryItems = inventoryItemsResolver
                           , workQueues = workQueueResolver
                           , workOrders = workOrderResolver
                           }


-- BASE EXAMPLE
-- https://github.com/dnulnets/haccessability
--dbFetchDeity:: Text -> Handler Deity
--dbFetchDeity name = do
--                     let userId = (toSqlKey 3)::User_Id
--                     deity <- runDB $ getEntity userId
--                     return $ Deity {fullName = "dummy", power = Just "Shapeshifting", tests = testsResolver}

--resolveDeity :: DeityArgs -> Res e Handler Deity
--resolveDeity DeityArgs { name, mythology } = lift $ dbFetchDeity name

--testsResolver :: TestArg -> Res e Handler NoDeity
--testsResolver TestArg {yourFullName } = pure NoDeity {noFullName = "Test no full am", nopower = Just "no power"}

rootResolver :: GQLRootResolver Handler () QueryQL Mutation Undefined
rootResolver = GQLRootResolver { queryResolver = resolveQuery
                               , mutationResolver = resolveMutation
                               , subscriptionResolver = Undefined
                               }

-- | Compose the graphQL api
api:: GQLRequest -> Handler GQLResponse
api r = do
         interpreter rootResolver r

apiDoc :: Data.ByteString.Lazy.Internal.ByteString
apiDoc = toGraphQLDocument $ Just rootResolver
