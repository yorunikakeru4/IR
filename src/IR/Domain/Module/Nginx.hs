{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.Nginx (
    NginxConfig (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (renderDomainError)
import IR.Domain.Module (ModuleName, mkModuleName, moduleNameText)

data NginxConfig = NginxConfig
    { nginxConfigName :: ModuleName
    , nginxEnable :: Bool
    , nginxHttpPort :: Maybe Int
    , nginxHttpsPort :: Maybe Int
    , nginxDomain :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON NginxConfig where
    toJSON nc =
        object $
            ["name" .= moduleNameText (nginxConfigName nc), "enable" .= nginxEnable nc]
                <> maybe [] (\p -> ["http_port" .= p]) (nginxHttpPort nc)
                <> maybe [] (\p -> ["https_port" .= p]) (nginxHttpsPort nc)
                <> maybe [] (\d -> ["domain" .= d]) (nginxDomain nc)

instance FromJSON NginxConfig where
    parseJSON = withObject "NginxConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        NginxConfig name
            <$> o .: "enable"
            <*> o .:? "http_port"
            <*> o .:? "https_port"
            <*> o .:? "domain"
