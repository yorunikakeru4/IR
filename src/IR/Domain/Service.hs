{-# LANGUAGE OverloadedStrings #-}

-- | Service-related IR nodes: actions for managing system services.
module IR.Domain.Service (
    ServiceName (..),
    mkServiceName,
    serviceNameText,
    Action (..),
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (DomainError, mkName)

-- | Name of a system service (e.g. systemd unit name without the @.service@ suffix).
newtype ServiceName = ServiceName Text
    deriving (Eq, Show)

mkServiceName :: Text -> Either DomainError ServiceName
mkServiceName = mkName ServiceName

serviceNameText :: ServiceName -> Text
serviceNameText (ServiceName n) = n

-- | Service-domain actions.
data Action
    = Enable ServiceName
    | Disable ServiceName
    deriving (Eq, Show)

instance ToJSON Action where
    toJSON (Disable (ServiceName n)) =
        object ["type" .= ("service_disable" :: Text), "name" .= n]
    toJSON (Enable (ServiceName n)) =
        object ["type" .= ("service_enable" :: Text), "name" .= n]

instance FromJSON Action where
    parseJSON = withObject "Service.Action" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "service_disable" -> do
                raw <- o .: "name"
                name <- either (fail . show) pure (mkServiceName raw)
                pure (Disable name)
            "service_enable" -> do
                raw <- o .: "name"
                name <- either (fail . show) pure (mkServiceName raw)
                pure (Enable name)
            _unknownType -> fail $ "unknown service action: " <> T.unpack t
