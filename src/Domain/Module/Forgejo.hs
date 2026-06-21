{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.Forgejo (
    ForgejoConfig (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (renderDomainError)
import Domain.Module (ModuleName, mkModuleName, moduleNameText)
import Policy (Policy)

-- Raw forgejo configuration, for
data ForgejoConfig = ForgejoConfig
    { configName :: ModuleName
    , enable :: Bool
    , httpPort :: Maybe Int
    , sshPort :: Maybe Int
    , domain :: Maybe Text
    , policies :: [Policy]
    }
    deriving (Eq, Show)

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
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        ForgejoConfig name
            <$> o .: "enable"
            <*> o .:? "http_port"
            <*> o .:? "ssh_port"
            <*> o .:? "domain"
            <*> o .:? "policies" .!= []
