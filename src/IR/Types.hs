{-# LANGUAGE OverloadedStrings #-}

{- | Core IR document structure: the top-level intent representation
consumed by the FrogOS Planner.
-}
module IR.Types (
    IRVersion (..),
    currentIRVersion,
    ProfileName,
    mkProfileName,
    profileNameText,
    ServiceSectionName,
    mkServiceSectionName,
    serviceSectionNameText,
    ProfileSection (..),
    ServiceSection (..),
    ModuleMap (..),
    emptyModuleMap,
    mmIsEmpty,
    IRDocument (..),
    PackageName,
    mkPackageName,
    packageNameText,
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import IR.Action (Action)
import IR.Condition (Condition)
import IR.Domain.Error (DomainError, mkName, renderDomainError)
import IR.Domain.Module.Docker (DockerConfig)
import IR.Domain.Module.Forgejo (ForgejoConfig)
import IR.Domain.Module.Nginx (NginxConfig)
import IR.Domain.Module.PostgreSQL (PostgreSQLConfig)
import IR.Domain.Package (PackageName, mkPackageName, packageNameText)
import qualified IR.Domain.Service as Service
import IR.Policy (Policy)

{- | Monotonically incrementing IR schema version.
A version mismatch causes the engine to reject the document and roll back.
-}
newtype IRVersion = IRVersion Int
    deriving (Eq, Show)

{- | The current IR schema version. Bump this whenever the 'IR.Condition.Condition'
or 'IR.Action.Action' sums are extended with new constructors.
-}
currentIRVersion :: IRVersion
currentIRVersion = IRVersion 3

-- | User-defined name for a profile section.
newtype ProfileName = ProfileName Text
    deriving (Eq, Show)

-- | User-defined name for a service section.
newtype ServiceSectionName = ServiceSectionName Text
    deriving (Eq, Show)

mkProfileName :: Text -> Either DomainError ProfileName
mkProfileName = mkName ProfileName

profileNameText :: ProfileName -> Text
profileNameText (ProfileName name) = name

mkServiceSectionName :: Text -> Either DomainError ServiceSectionName
mkServiceSectionName = mkName ServiceSectionName

serviceSectionNameText :: ServiceSectionName -> Text
serviceSectionNameText (ServiceSectionName name) = name

-- | A named group of conditions, policies, and actions applied when the profile is active.
data ProfileSection = ProfileSection
    { profileName :: ProfileName
    , profileCondition :: Maybe Condition
    , profilePolicies :: [Policy]
    , profileActions :: [Action]
    , profilePackages :: [PackageName]
    }
    deriving (Eq, Show)

-- | A named group of base state, policies, and actions for a service.
data ServiceSection = ServiceSection
    { serviceSectionName :: ServiceSectionName
    , serviceSectionBaseState :: Maybe Service.ServiceBaseState
    , serviceSectionPolicies :: [Policy]
    , serviceSectionActions :: [Action]
    , serviceSectionPackages :: [PackageName]
    }
    deriving (Eq, Show)

data ModuleMap = ModuleMap
    { mmDocker :: [DockerConfig]
    , mmPostgreSQL :: [PostgreSQLConfig]
    , mmNginx :: [NginxConfig]
    , mmForgejo :: [ForgejoConfig]
    }
    deriving (Eq, Show)

emptyModuleMap :: ModuleMap
emptyModuleMap = ModuleMap{mmDocker = [], mmPostgreSQL = [], mmNginx = [], mmForgejo = []}

mmIsEmpty :: ModuleMap -> Bool
mmIsEmpty mm = null (mmDocker mm) && null (mmPostgreSQL mm) && null (mmNginx mm) && null (mmForgejo mm)

instance ToJSON ModuleMap where
    toJSON mm =
        object $
            omitEmpty "docker" (mmDocker mm)
                <> omitEmpty "postgresql" (mmPostgreSQL mm)
                <> omitEmpty "nginx" (mmNginx mm)
                <> omitEmpty "forgejo" (mmForgejo mm)

instance FromJSON ModuleMap where
    parseJSON = withObject "ModuleMap" $ \o ->
        ModuleMap
            <$> o .:? "docker" .!= []
            <*> o .:? "postgresql" .!= []
            <*> o .:? "nginx" .!= []
            <*> o .:? "forgejo" .!= []

-- | The complete IR document serialized as @intent.json@ for each generation.
data IRDocument = IRDocument
    { irVersion :: IRVersion
    , irPackages :: [PackageName]
    , irModules :: ModuleMap
    , irProfiles :: [ProfileSection]
    , irServices :: [ServiceSection]
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
        either (fail . T.unpack . renderDomainError) pure (mkProfileName name)

instance ToJSON ServiceSectionName where
    toJSON (ServiceSectionName t) = toJSON t

instance FromJSON ServiceSectionName where
    parseJSON value = do
        name <- parseJSON value
        either (fail . T.unpack . renderDomainError) pure (mkServiceSectionName name)

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

instance ToJSON ServiceSection where
    toJSON ss =
        object $
            [ "name" .= serviceSectionName ss
            , "policies" .= serviceSectionPolicies ss
            , "actions" .= serviceSectionActions ss
            ]
                <> maybe [] (\b -> ["baseState" .= b]) (serviceSectionBaseState ss)
                <> omitEmpty "packages" (serviceSectionPackages ss)

instance FromJSON ServiceSection where
    parseJSON = withObject "ServiceSection" $ \o ->
        ServiceSection
            <$> o .: "name"
            <*> o .:? "baseState"
            <*> o .: "policies"
            <*> o .: "actions"
            <*> o .:? "packages" .!= []

instance ToJSON IRDocument where
    toJSON doc =
        object $
            ["version" .= irVersion doc, "profiles" .= irProfiles doc, "services" .= irServices doc]
                <> omitEmpty "packages" (irPackages doc)
                <> (if mmIsEmpty (irModules doc) then [] else ["modules" .= irModules doc])

instance FromJSON IRDocument where
    parseJSON = withObject "IRDocument" $ \o ->
        IRDocument
            <$> o .: "version"
            <*> o .:? "packages" .!= []
            <*> o .:? "modules" .!= emptyModuleMap
            <*> o .: "profiles"
            <*> o .: "services"
