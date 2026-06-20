{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.Forgejo (
    ForgejoConfig (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (renderDomainError)
import Domain.Module (ModuleName, mkModuleName, moduleNameText)

data ForgejoConfig = ForgejoConfig
    { forgejoConfigName :: ModuleName
    , forgejoEnable :: Bool
    , forgejoHttpPort :: Maybe Int
    , forgejoSshPort :: Maybe Int
    , forgejoDomain :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON ForgejoConfig where
    toJSON fc =
        object $
            ["name" .= moduleNameText (forgejoConfigName fc), "enable" .= forgejoEnable fc]
                <> maybe [] (\p -> ["http_port" .= p]) (forgejoHttpPort fc)
                <> maybe [] (\p -> ["ssh_port" .= p]) (forgejoSshPort fc)
                <> maybe [] (\d -> ["domain" .= d]) (forgejoDomain fc)

instance FromJSON ForgejoConfig where
    parseJSON = withObject "ForgejoConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        ForgejoConfig name
            <$> o .: "enable"
            <*> o .:? "http_port"
            <*> o .:? "ssh_port"
            <*> o .:? "domain"
