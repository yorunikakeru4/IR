{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.Nginx.VirtualHost (
    NginxVirtualHost (..),
) where

import Data.Aeson
import Data.Text (Text)

data NginxVirtualHost = NginxVirtualHost
    { vhostDomain :: Text
    , vhostHttpPort :: Maybe Int
    , vhostHttpsPort :: Maybe Int
    , vhostProxyPass :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON NginxVirtualHost where
    toJSON vh =
        object $
            ["domain" .= vhostDomain vh]
                <> maybe [] (\p -> ["http_port" .= p]) (vhostHttpPort vh)
                <> maybe [] (\p -> ["https_port" .= p]) (vhostHttpsPort vh)
                <> maybe [] (\u -> ["proxy_pass" .= u]) (vhostProxyPass vh)

instance FromJSON NginxVirtualHost where
    parseJSON = withObject "NginxVirtualHost" $ \o ->
        NginxVirtualHost
            <$> o .: "domain"
            <*> o .:? "http_port"
            <*> o .:? "https_port"
            <*> o .:? "proxy_pass"
