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
) where

import Data.Aeson
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

data ModuleRef = ModuleRef
    { moduleRefDomain :: ModuleDomain
    , moduleRefName :: ModuleName
    }
    deriving (Eq, Show)

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
