{-# LANGUAGE OverloadedStrings #-}

module Domain.Module (
    ModuleDomain (..),
    moduleDomainText,
    moduleDomainName,
    ModuleName,
    mkModuleName,
    moduleNameText,
    ModuleRef (..),
    LifecycleAction (..),
    InstallAction (..),
    ConfigureAction (..),
    UnconfigureAction (..),
) where

import Data.Aeson (FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (DomainError, mkName, parseDomain)

data ModuleDomain
    = PostgreSQLDomain
    | NginxDomain
    | ForgejoDomain
    deriving (Eq, Ord, Show)

moduleDomainText :: ModuleDomain -> Text
moduleDomainText PostgreSQLDomain = "postgresql"
moduleDomainText NginxDomain = "nginx"
moduleDomainText ForgejoDomain = "forgejo"

moduleDomainName :: ModuleDomain -> ModuleName
moduleDomainName = ModuleName . moduleDomainText

instance ToJSON ModuleDomain where
    toJSON = toJSON . moduleDomainText

instance FromJSON ModuleDomain where
    parseJSON = withText "ModuleDomain" $ \t -> case t of
        "postgresql" -> pure PostgreSQLDomain
        "nginx" -> pure NginxDomain
        "forgejo" -> pure ForgejoDomain
        _unknown -> fail $ "unknown module domain: " <> T.unpack t

newtype ModuleName = ModuleName Text
    deriving (Eq, Ord, Show)

mkModuleName :: Text -> Either DomainError ModuleName
mkModuleName = mkName ModuleName

moduleNameText :: ModuleName -> Text
moduleNameText (ModuleName n) = n

instance ToJSON ModuleName where
    toJSON (ModuleName n) = toJSON n

instance FromJSON ModuleName where
    parseJSON value = do
        name <- parseJSON value
        parseDomain (mkModuleName name)

-- Eq, Ord, Show
data ModuleRef = ModuleRef
    { moduleRefDomain :: ModuleDomain
    , moduleRefName :: ModuleName
    }
    deriving (Eq, Ord, Show)

instance ToJSON ModuleRef where
    toJSON ref =
        object
            [ "domain" .= moduleRefDomain ref
            , "name" .= moduleRefName ref
            ]

instance FromJSON ModuleRef where
    parseJSON = withObject "ModuleRef" $ \o ->
        ModuleRef
            <$> o .: "domain"
            <*> o .: "name"

data LifecycleAction
    = EnableModule ModuleRef Int
    | DisableModule ModuleRef Int
    | RestartModule ModuleRef Int
    deriving (Eq, Show)

instance ToJSON LifecycleAction where
    toJSON (EnableModule ref p) =
        object ["type" .= ("module_enable" :: Text), "module" .= ref, "priority" .= p]
    toJSON (DisableModule ref p) =
        object ["type" .= ("module_disable" :: Text), "module" .= ref, "priority" .= p]
    toJSON (RestartModule ref p) =
        object ["type" .= ("module_restart" :: Text), "module" .= ref, "priority" .= p]

instance FromJSON LifecycleAction where
    parseJSON = withObject "Module.LifecycleAction" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "module_enable" -> EnableModule <$> o .: "module" <*> o .:? "priority" .!= 100
            "module_disable" -> DisableModule <$> o .: "module" <*> o .:? "priority" .!= 100
            "module_restart" -> RestartModule <$> o .: "module" <*> o .:? "priority" .!= 100
            _unknown -> fail $ "unknown module action type: " <> T.unpack t

data InstallAction = InstallModule ModuleRef
    deriving (Eq, Show)

data ConfigureAction = ConfigureModule
    { configureRef :: ModuleRef
    , configureHash :: Text
    , configureJson :: Value
    }
    deriving (Eq, Show)

data UnconfigureAction = UnconfigureModule ModuleRef
    deriving (Eq, Show)

instance ToJSON InstallAction where
    toJSON (InstallModule ref) =
        object ["type" .= ("module_install" :: Text), "module" .= ref]

instance FromJSON InstallAction where
    parseJSON = withObject "InstallAction" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "module_install" -> InstallModule <$> o .: "module"
            _unknown -> fail $ "unknown install action type: " <> T.unpack t

instance ToJSON ConfigureAction where
    toJSON ca =
        object
            [ "type" .= ("module_configure" :: Text)
            , "module" .= configureRef ca
            , "config_hash" .= configureHash ca
            , "config_json" .= configureJson ca
            ]

instance FromJSON ConfigureAction where
    parseJSON = withObject "ConfigureAction" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "module_configure" ->
                ConfigureModule
                    <$> o .: "module"
                    <*> o .: "config_hash"
                    <*> o .: "config_json"
            _unknown -> fail $ "unknown configure action type: " <> T.unpack t

instance ToJSON UnconfigureAction where
    toJSON (UnconfigureModule ref) =
        object ["type" .= ("module_unconfigure" :: Text), "module" .= ref]

instance FromJSON UnconfigureAction where
    parseJSON = withObject "UnconfigureAction" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "module_unconfigure" -> UnconfigureModule <$> o .: "module"
            _unknown -> fail $ "unknown unconfigure action type: " <> T.unpack t
