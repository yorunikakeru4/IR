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
    IRDocument (..),
    PackageName,
    mkPackageName,
    packageNameText,
) where

import Action (Action)
import Condition (Condition)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (DomainError, mkName, renderDomainError)
import Domain.Module.Docker (DockerConfig)
import Domain.Module.Forgejo (ForgejoConfig)
import Domain.Module.Nginx (NginxConfig)
import Domain.Module.PostgreSQL (PostgreSQLConfig)
import Domain.Package (PackageName, mkPackageName, packageNameText)
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
currentIRVersion = IRVersion 5

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

instance FromJSON IRDocument where
    parseJSON = withObject "IRDocument" $ \o ->
        IRDocument
            <$> o .: "version"
            <*> o .:? "packages" .!= []
            <*> o .:? "modules" .!= emptyModuleMap
            <*> o .: "profiles"
