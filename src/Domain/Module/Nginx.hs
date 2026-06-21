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
    { configName :: ModuleName
    , enable :: Bool
    , virtualHosts :: [NginxVirtualHost]
    , policies :: [Policy]
    }
    deriving (Eq, Show)

instance ToJSON NginxConfig where
    toJSON nc =
        object $
            ["name" .= moduleNameText (configName nc), "enable" .= enable nc]
                <> (if null (virtualHosts nc) then [] else ["virtual_hosts" .= virtualHosts nc])
                <> (if null (policies nc) then [] else ["policies" .= policies nc])

instance FromJSON NginxConfig where
    parseJSON = withObject "NginxConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        NginxConfig name
            <$> o .: "enable"
            <*> o .:? "virtual_hosts" .!= []
            <*> o .:? "policies" .!= []
