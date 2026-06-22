{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.Forgejo (
    ForgejoConfig (..),
    emptyForgejoConfig,
) where

import Data.Aeson
import Data.Text (Text)
import Domain.Error (parseDomain)
import Domain.Module (ModuleDomain (ForgejoDomain), ModuleName, mkModuleName, moduleDomainName, moduleNameText)
import Policy (Policy)

data ForgejoConfig = ForgejoConfig
    { configName :: ModuleName
    , enable :: Bool
    , httpPort :: Maybe Int
    , sshPort :: Maybe Int
    , domain :: Maybe Text
    , policies :: [Policy]
    }
    deriving (Eq, Show)

emptyForgejoConfig :: ForgejoConfig
emptyForgejoConfig =
    ForgejoConfig
        { configName = moduleDomainName ForgejoDomain
        , enable = True
        , httpPort = Nothing
        , sshPort = Nothing
        , domain = Nothing
        , policies = []
        }

instance ToJSON ForgejoConfig where
    toJSON fc =
        object $
            ["name" .= moduleNameText (configName fc), "enable" .= enable fc]
                <> maybe [] (\p -> ["http_port" .= p]) (httpPort fc)
                <> maybe [] (\p -> ["ssh_port" .= p]) (sshPort fc)
                <> maybe [] (\d -> ["domain" .= d]) (domain fc)
                <> (if null (policies fc) then [] else ["policies" .= policies fc])

instance FromJSON ForgejoConfig where
    parseJSON = withObject "ForgejoConfig" $ \o -> do
        rawName <- o .: "name"
        name <- parseDomain (mkModuleName rawName)
        ForgejoConfig name
            <$> o .: "enable"
            <*> o .:? "http_port"
            <*> o .:? "ssh_port"
            <*> o .:? "domain"
            <*> o .:? "policies" .!= []
