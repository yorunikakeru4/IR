{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
    ProfileBody (..),
    ModulesConfig (..),
    emptyModules,
    modulesEmpty,
    IRDocument (..),
    emptyIRDocument,
    PackageName,
    mkPackageName,
    packageNameText,
    emptyProfileBody,
) where

import Action (Action)
import Condition (Condition)
import Data.Aeson
import Data.Text (Text)
import Domain.Error (DomainError, mkName, parseDomain)
import qualified Domain.Module as Module
import Domain.Module.Forgejo (ForgejoConfig)
import qualified Domain.Module.Forgejo as Forgejo
import Domain.Module.Nginx (NginxConfig)
import qualified Domain.Module.Nginx as Nginx
import Domain.Module.PostgreSQL (PostgreSQLConfig)
import qualified Domain.Module.PostgreSQL as PostgreSQL
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
currentIRVersion = IRVersion 9

-- | User-defined name for a profile section.
newtype ProfileName = ProfileName Text
    deriving (Eq, Show)

mkProfileName :: Text -> Either DomainError ProfileName
mkProfileName = mkName ProfileName

profileNameText :: ProfileName -> Text
profileNameText (ProfileName name) = name

data ProfileBody = ProfileBody
    { pbCondition :: Maybe Condition
    , pbPolicies :: [Policy]
    , pbActions :: [Action]
    , pbPackages :: [PackageName]
    }
    deriving (Eq, Show)

emptyProfileBody :: ProfileBody
emptyProfileBody = ProfileBody{pbCondition = Nothing, pbPolicies = [], pbActions = [], pbPackages = []}

instance ToJSON ProfileBody where
    toJSON pb =
        object $
            ["policies" .= pbPolicies pb, "actions" .= pbActions pb]
                <> maybe [] (\c -> ["condition" .= c]) (pbCondition pb)
                <> omitEmpty "packages" (pbPackages pb)

instance FromJSON ProfileBody where
    parseJSON = withObject "ProfileBody" $ \o ->
        ProfileBody
            <$> o .:? "condition"
            <*> o .:? "policies" .!= []
            <*> o .:? "actions" .!= []
            <*> o .:? "packages" .!= []

data ModulesConfig = ModulesConfig
    { mmPostgreSQL :: Maybe PostgreSQLConfig
    , mmNginx :: Maybe NginxConfig
    , mmForgejo :: Maybe ForgejoConfig
    }
    deriving (Eq, Show)

emptyModules :: ModulesConfig
emptyModules = ModulesConfig{mmPostgreSQL = Nothing, mmNginx = Nothing, mmForgejo = Nothing}

modulesEmpty :: ModulesConfig -> Bool
modulesEmpty (ModulesConfig Nothing Nothing Nothing) = True
modulesEmpty _ = False

omitNothing :: (ToJSON a, KeyValue e kv) => Key -> Maybe a -> [kv]
omitNothing _ Nothing = []
omitNothing key (Just x) = [key .= x]

instance ToJSON ModulesConfig where
    toJSON mm =
        object $
            omitNothing "postgresql" (mmPostgreSQL mm)
                <> omitNothing "nginx" (mmNginx mm)
                <> omitNothing "forgejo" (mmForgejo mm)

instance FromJSON ModulesConfig where
    parseJSON = withObject "Modules" $ \o ->
        ModulesConfig
            <$> o .:? "postgresql"
            <*> o .:? "nginx"
            <*> o .:? "forgejo"

-- | The complete IR document serialized as @intent.json@ for each generation.
data IRDocument = IRDocument
    { irVersion :: IRVersion
    , irPackages :: [PackageName]
    , irModules :: ModulesConfig
    , irProfiles :: [ProfileSection]
    , irUsers :: [UserConfig]
    , irNetwork :: NetworkConfig
    }
    deriving (Eq, Show)

-- | Empty IR document with the current schema version and no entries.
emptyIRDocument :: IRDocument
emptyIRDocument =
    IRDocument
        { irVersion = currentIRVersion
        , irPackages = []
        , irModules = emptyModules
        , irProfiles = []
        , irUsers = []
        , irNetwork = emptyNetworkConfig
        }

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

data ProfileSection = ProfileSection
    { profileName :: ProfileName
    , profileBody :: ProfileBody
    }
    deriving (Eq, Show)

instance ToJSON ProfileSection where
    toJSON ps =
        case toJSON (profileBody ps) of
            Object body ->
                Object $ body <> "name" .= profileName ps
            _error ->
                error "ProfileBody must encode to JSON object"

instance FromJSON ProfileSection where
    parseJSON = withObject "ProfileSection" $ \o ->
        ProfileSection
            <$> o .: "name"
            <*> parseJSON (Object o)

instance ToJSON IRDocument where
    toJSON doc =
        object $
            ["version" .= irVersion doc, "profiles" .= irProfiles doc]
                <> omitEmpty "packages" (irPackages doc)
                <> (if modulesEmpty (irModules doc) then [] else ["modules" .= irModules doc])
                <> omitEmpty "users" (irUsers doc)
                <> (if null (networkAllowPorts (irNetwork doc)) then [] else ["network" .= irNetwork doc])

instance FromJSON IRDocument where
    parseJSON = withObject "IRDocument" $ \o ->
        IRDocument
            <$> o .: "version"
            <*> o .:? "packages" .!= []
            <*> o .:? "modules" .!= emptyModules
            <*> o .: "profiles"
            <*> o .:? "users" .!= []
            <*> o .:? "network" .!= emptyNetworkConfig
