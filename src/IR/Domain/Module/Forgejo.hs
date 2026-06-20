{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.Forgejo (
    ForgejoConfig (..),
) where

import Data.Aeson
import qualified Data.Text as T
import IR.Domain.Error (renderDomainError)
import IR.Domain.Module (ModuleName, mkModuleName, moduleNameText)

data ForgejoConfig = ForgejoConfig
    { forgejoConfigName :: ModuleName
    , forgejoEnable :: Bool
    }
    deriving (Eq, Show)

instance ToJSON ForgejoConfig where
    toJSON fc =
        object
            [ "name" .= moduleNameText (forgejoConfigName fc)
            , "enable" .= forgejoEnable fc
            ]

instance FromJSON ForgejoConfig where
    parseJSON = withObject "ForgejoConfig" $ \o -> do
        rawName <- o .: "name"
        name <- either (fail . T.unpack . renderDomainError) pure (mkModuleName rawName)
        ForgejoConfig name <$> o .: "enable"
