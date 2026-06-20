{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.Nginx (
    NginxConfig (..),
) where

import Data.Aeson
import qualified Data.Text as T
import Domain.Error (renderDomainError)
import Domain.Module (ModuleName, mkModuleName, moduleNameText)
import Domain.Module.Nginx.VirtualHost (NginxVirtualHost)
import Policy (Policy)

data NginxConfig = NginxConfig
    { nginxConfigName :: ModuleName
    , nginxEnable :: Bool
    , nginxVirtualHosts :: [NginxVirtualHost]
    , nginxPolicies :: [Policy]
    }
    deriving (Eq, Show)

instance ToJSON NginxConfig where
    toJSON nc =
        object $
            ["name" .= moduleNameText (nginxConfigName nc), "enable" .= nginxEnable nc]
                <> (if null (nginxVirtualHosts nc) then [] else ["virtual_hosts" .= nginxVirtualHosts nc])
                <> (if null (nginxPolicies nc) then [] else ["policies" .= nginxPolicies nc])

instance FromJSON NginxConfig where
    parseJSON = withObject "NginxConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        NginxConfig name
            <$> o .: "enable"
            <*> o .:? "virtual_hosts" .!= []
            <*> o .:? "policies" .!= []
