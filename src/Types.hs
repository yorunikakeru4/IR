{-# LANGUAGE OverloadedStrings #-}

{- | Core IR document structure: the top-level intent representation
consumed by the FrogOS Planner.
-}
module Types (
    IRVersion (..),
    currentIRVersion,
    ProfileName,
    mkProfileName,
    profileNameText,
    ProfileSection (..),
    ModuleMap (..),
    emptyModuleMap,
    mmIsEmpty,
    moduleMapEntries,
    IRDocument (..),
    PackageName,
    mkPackageName,
    packageNameText,
) where

import Action (Action)
import Condition (Condition)
import Data.Aeson
import Data.Text (Text)
import Domain.Error (DomainError, mkName, parseDomain)
import qualified Domain.Module as Module
import qualified Domain.Module.Forgejo as Forgejo
import Domain.Module.Forgejo (ForgejoConfig)
import qualified Domain.Module.Nginx as Nginx
import Domain.Module.Nginx (NginxConfig)
import qualified Domain.Module.PostgreSQL as PostgreSQL
import Domain.Module.PostgreSQL (PostgreSQLConfig)
import Domain.Network (NetworkConfig, emptyNetworkConfig, networkAllowPorts)
import Domain.Package (PackageName, mkPackageName, packageNameText)
import Domain.User (UserConfig)
import Policy (Policy)

{- | Monotonically incrementing IR schema version.
A version mismatch causes the engine to reject the document and roll back.
-}
newtype IRVersion = IRVersion Int
    deriving (Eq, Show)

{- | The current IR schema version. Bump this whenever the public IR JSON schema
changes, including condition/action sums and module config shapes.
-}
currentIRVersion :: IRVersion
currentIRVersion = IRVersion 8

-- | User-defined name for a profile section.
newtype ProfileName = ProfileName Text
    deriving (Eq, Show)

mkProfileName :: Text -> Either DomainError ProfileName
mkProfileName = mkName ProfileName

profileNameText :: ProfileName -> Text
profileNameText (ProfileName name) = name

-- | A named group of conditions, policies, and actions applied when the profile is active.
data ProfileSection = ProfileSection
    { profileName :: ProfileName
    , profileCondition :: Maybe Condition
    , profilePolicies :: [Policy]
    , profileActions :: [Action]
    , profilePackages :: [PackageName]
    }
    deriving (Eq, Show)

data ModuleMap = ModuleMap
    { mmPostgreSQL :: [PostgreSQLConfig]
    , mmNginx :: [NginxConfig]
    , mmForgejo :: [ForgejoConfig]
    }
    deriving (Eq, Show)

emptyModuleMap :: ModuleMap
emptyModuleMap = ModuleMap{mmPostgreSQL = [], mmNginx = [], mmForgejo = []}

mmIsEmpty :: ModuleMap -> Bool
mmIsEmpty mm = null (mmPostgreSQL mm) && null (mmNginx mm) && null (mmForgejo mm)

moduleMapEntries :: ModuleMap -> [(Module.ModuleDomain, Module.ModuleName)]
moduleMapEntries mm =
    [(Module.PostgreSQLDomain, PostgreSQL.configName cfg) | cfg <- mmPostgreSQL mm]
        <> [(Module.NginxDomain, Nginx.configName cfg) | cfg <- mmNginx mm]
        <> [(Module.ForgejoDomain, Forgejo.configName cfg) | cfg <- mmForgejo mm]

instance ToJSON ModuleMap where
    toJSON mm =
        object $
            omitEmpty "postgresql" (mmPostgreSQL mm)
                <> omitEmpty "nginx" (mmNginx mm)
                <> omitEmpty "forgejo" (mmForgejo mm)

instance FromJSON ModuleMap where
    parseJSON = withObject "ModuleMap" $ \o ->
        ModuleMap
            <$> o .:? "postgresql" .!= []
            <*> o .:? "nginx" .!= []
            <*> o .:? "forgejo" .!= []

-- | The complete IR document serialized as @intent.json@ for each generation.
data IRDocument = IRDocument
    { irVersion :: IRVersion
    , irPackages :: [PackageName]
    , irModules :: ModuleMap
    , irProfiles :: [ProfileSection]
    , irUsers :: [UserConfig]
    , irNetwork :: NetworkConfig
    }
    deriving (Eq, Show)

instance ToJSON IRVersion where
    toJSON (IRVersion n) = toJSON n

instance FromJSON IRVersion where
    parseJSON = fmap IRVersion . parseJSON

instance ToJSON ProfileName where
    toJSON (ProfileName t) = toJSON t

instance FromJSON ProfileName where
    parseJSON value = do
        name <- parseJSON value
        parseDomain (mkProfileName name)

-- | Serialise a list field, omitting the key entirely when the list is empty.
omitEmpty :: (ToJSON a, KeyValue e kv) => Key -> [a] -> [kv]
omitEmpty _ [] = []
omitEmpty key xs = [key .= xs]

instance ToJSON ProfileSection where
    toJSON ps =
        object $
            ["name" .= profileName ps, "policies" .= profilePolicies ps, "actions" .= profileActions ps]
                <> maybe [] (\c -> ["condition" .= c]) (profileCondition ps)
                <> omitEmpty "packages" (profilePackages ps)

instance FromJSON ProfileSection where
    parseJSON = withObject "ProfileSection" $ \o ->
        ProfileSection
            <$> o .: "name"
            <*> o .:? "condition"
            <*> o .: "policies"
            <*> o .: "actions"
            <*> o .:? "packages" .!= []

instance ToJSON IRDocument where
    toJSON doc =
        object $
            ["version" .= irVersion doc, "profiles" .= irProfiles doc]
                <> omitEmpty "packages" (irPackages doc)
                <> (if mmIsEmpty (irModules doc) then [] else ["modules" .= irModules doc])
                <> omitEmpty "users" (irUsers doc)
                <> (if null (networkAllowPorts (irNetwork doc)) then [] else ["network" .= irNetwork doc])

instance FromJSON IRDocument where
    parseJSON = withObject "IRDocument" $ \o ->
        IRDocument
            <$> o .:  "version"
            <*> o .:? "packages" .!= []
            <*> o .:? "modules"  .!= emptyModuleMap
            <*> o .:  "profiles"
            <*> o .:? "users"    .!= []
            <*> o .:? "network"  .!= emptyNetworkConfig
