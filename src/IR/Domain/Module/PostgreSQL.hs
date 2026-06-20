{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.PostgreSQL (
    PostgreSQLConfig (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (renderDomainError)
import IR.Domain.Module (ModuleName, mkModuleName, moduleNameText)

data PostgreSQLConfig = PostgreSQLConfig
    { postgresqlConfigName :: ModuleName
    , postgresqlEnable :: Bool
    , postgresqlPort :: Maybe Int
    , postgresqlDataDir :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON PostgreSQLConfig where
    toJSON pc =
        object $
            ["name" .= moduleNameText (postgresqlConfigName pc), "enable" .= postgresqlEnable pc]
                <> maybe [] (\p -> ["port" .= p]) (postgresqlPort pc)
                <> maybe [] (\d -> ["data_dir" .= d]) (postgresqlDataDir pc)

instance FromJSON PostgreSQLConfig where
    parseJSON = withObject "PostgreSQLConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        PostgreSQLConfig name
            <$> o .: "enable"
            <*> o .:? "port"
            <*> o .:? "data_dir"
