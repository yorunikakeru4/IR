{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.Virtualization (
    VirtualizationConfig (..),
    emptyVirtualizationConfig,
) where

import Data.Aeson
import Domain.Error (parseDomain)
import Domain.Module (ModuleDomain (VirtualizationDomain), ModuleName, mkModuleName, moduleDomainName, moduleNameText)

data VirtualizationConfig = VirtualizationConfig
    { configName :: ModuleName
    , enable :: Bool
    }
    deriving (Eq, Show)

emptyVirtualizationConfig :: VirtualizationConfig
emptyVirtualizationConfig =
    VirtualizationConfig
        { configName = moduleDomainName VirtualizationDomain
        , enable = True
        }

instance ToJSON VirtualizationConfig where
    toJSON vc =
        object
            [ "name" .= moduleNameText (configName vc)
            , "enable" .= enable vc
            ]

instance FromJSON VirtualizationConfig where
    parseJSON = withObject "VirtualizationConfig" $ \o -> do
        rawName <- o .: "name"
        name <- parseDomain (mkModuleName rawName)
        VirtualizationConfig name
            <$> o .: "enable"
