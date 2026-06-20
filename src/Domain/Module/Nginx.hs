{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.Nginx (
    NginxConfig (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (renderDomainError)
import Domain.Module (ModuleName, mkModuleName, moduleNameText)
import Domain.Module.Nginx.VirtualHost (NginxVirtualHost)

data NginxConfig = NginxConfig
    { nginxConfigName :: ModuleName
    , nginxEnable :: Bool
    , nginxHttpPort :: Maybe Int
    , nginxHttpsPort :: Maybe Int
    , nginxDomain :: Maybe Text
    , nginxVirtualHosts :: [NginxVirtualHost]
    }
    deriving (Eq, Show)

instance ToJSON NginxConfig where
    toJSON nc =
        object $
            ["name" .= moduleNameText (nginxConfigName nc), "enable" .= nginxEnable nc]
                <> maybe [] (\p -> ["http_port" .= p]) (nginxHttpPort nc)
                <> maybe [] (\p -> ["https_port" .= p]) (nginxHttpsPort nc)
                <> maybe [] (\d -> ["domain" .= d]) (nginxDomain nc)
                <> (if null (nginxVirtualHosts nc) then [] else ["virtual_hosts" .= nginxVirtualHosts nc])

instance FromJSON NginxConfig where
    parseJSON = withObject "NginxConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        NginxConfig name
            <$> o .: "enable"
            <*> o .:? "http_port"
            <*> o .:? "https_port"
            <*> o .:? "domain"
            <*> o .:? "virtual_hosts" .!= []
