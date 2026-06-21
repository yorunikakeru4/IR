{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.PostgreSQL (
    PostgreSQLConfig (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (renderDomainError)
import Domain.Module (ModuleName, mkModuleName, moduleNameText)
import Policy (Policy)

data PostgreSQLConfig = PostgreSQLConfig
    { configName :: ModuleName
    , enable :: Bool
    , port :: Maybe Int
    , dataDir :: Maybe Text
    , maxConnections :: Maybe Int
    , policies :: [Policy]
    }
    deriving (Eq, Show)

instance ToJSON PostgreSQLConfig where
    toJSON pc =
        object $
            ["name" .= moduleNameText (configName pc), "enable" .= enable pc]
                <> maybe [] (\p -> ["port" .= p]) (port pc)
                <> maybe [] (\d -> ["data_dir" .= d]) (dataDir pc)
                <> maybe [] (\m -> ["max_connections" .= m]) (maxConnections pc)
                <> (if null (policies pc) then [] else ["policies" .= policies pc])

instance FromJSON PostgreSQLConfig where
    parseJSON = withObject "PostgreSQLConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        PostgreSQLConfig name
            <$> o .: "enable"
            <*> o .:? "port"
            <*> o .:? "data_dir"
            <*> o .:? "max_connections"
            <*> o .:? "policies" .!= []
