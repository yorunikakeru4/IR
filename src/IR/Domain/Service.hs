{-# LANGUAGE OverloadedStrings #-}

-- | Service-related IR nodes: actions for managing system services.
module IR.Domain.Service (
    ServiceName (..),
    Action (..),
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T

-- | Name of a system service (e.g. systemd unit name without the @.service@ suffix).
newtype ServiceName = ServiceName Text
    deriving (Eq, Show)

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
            "service_disable" -> Disable . ServiceName <$> o .: "name"
            "service_enable" -> Enable . ServiceName <$> o .: "name"
            _unknownType -> fail $ "unknown service action: " <> T.unpack t
