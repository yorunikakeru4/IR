{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.Nginx.VirtualHost (
    NginxVirtualHost (..),
) where

import Data.Aeson
import Data.Text (Text)
import Policy (Policy)

data NginxVirtualHost = NginxVirtualHost
    { domain :: Text
    , httpPort :: Maybe Int
    , httpsPort :: Maybe Int
    , proxyPass :: Maybe Text
    , policies :: [Policy]
    }
    deriving (Eq, Show)

instance ToJSON NginxVirtualHost where
    toJSON vh =
        object $
            ["domain" .= domain vh]
                <> maybe [] (\p -> ["http_port" .= p]) (httpPort vh)
                <> maybe [] (\p -> ["https_port" .= p]) (httpsPort vh)
                <> maybe [] (\u -> ["proxy_pass" .= u]) (proxyPass vh)
                <> (if null (policies vh) then [] else ["policies" .= policies vh])

instance FromJSON NginxVirtualHost where
    parseJSON = withObject "NginxVirtualHost" $ \o ->
        NginxVirtualHost
            <$> o .: "domain"
            <*> o .:? "http_port"
            <*> o .:? "https_port"
            <*> o .:? "proxy_pass"
            <*> o .:? "policies" .!= []
