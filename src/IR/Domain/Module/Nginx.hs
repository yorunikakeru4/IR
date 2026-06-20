{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.Nginx (
    NginxConfig (..),
) where

import Data.Aeson
import qualified Data.Text as T
import IR.Domain.Error (renderDomainError)
import IR.Domain.Module (ModuleName, mkModuleName, moduleNameText)

data NginxConfig = NginxConfig
    { nginxConfigName :: ModuleName
    , nginxEnable :: Bool
    }
    deriving (Eq, Show)

instance ToJSON NginxConfig where
    toJSON nc =
        object
            [ "name" .= moduleNameText (nginxConfigName nc)
            , "enable" .= nginxEnable nc
            ]

instance FromJSON NginxConfig where
    parseJSON = withObject "NginxConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        NginxConfig name <$> o .: "enable"
