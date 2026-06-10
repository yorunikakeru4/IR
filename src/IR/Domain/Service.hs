{-# LANGUAGE OverloadedStrings #-}

-- | Service-related IR nodes: actions for managing system services.
module IR.Domain.Service (
    ServiceName (..),
    mkServiceName,
    serviceNameText,
    Priority (..),
    mkPriority,
    priorityInt,
    Action (..),
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (DomainError, mkName, mkPercent)
import Data.Maybe (fromMaybe)

-- | Name of a system service (e.g. systemd unit name without the @.service@ suffix).
newtype ServiceName = ServiceName Text
    deriving (Eq, Show)

mkServiceName :: Text -> Either DomainError ServiceName
mkServiceName = mkName ServiceName

serviceNameText :: ServiceName -> Text
serviceNameText (ServiceName n) = n

{- | Priority value in the range [0, 100]. Higher values take precedence when
two conflicting profile actions are simultaneously active.

The DSL's plain @enable@\/@disable@ combinators use priority 100. To allow a
conditional profile to be overridden by an unconditional one, use a priority
below 100.
-}
newtype Priority = Priority Int
    deriving (Eq, Ord, Show)

{- | Construct a 'Priority'. Valid range is [0, 100]; returns 'InvalidPercent'
for values outside that range.
-}
mkPriority :: Int -> Either DomainError Priority
mkPriority = mkPercent Priority

priorityInt :: Priority -> Int
priorityInt (Priority p) = p

-- | Service-domain actions. Every action carries an explicit priority.
data Action
    = EnableWithPriority ServiceName Priority
    | DisableWithPriority ServiceName Priority
    deriving (Eq, Show)

instance ToJSON Action where
    toJSON (EnableWithPriority (ServiceName n) p) =
        object
            [ "type" .= ("service_enable" :: Text)
            , "name" .= n
            , "priority" .= priorityInt p
            ]
    toJSON (DisableWithPriority (ServiceName n) p) =
        object
            [ "type" .= ("service_disable" :: Text)
            , "name" .= n
            , "priority" .= priorityInt p
            ]

instance FromJSON Action where
    parseJSON = withObject "Service.Action" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "service_enable" -> do
                raw <- o .: "name"
                name <- either (fail . show) pure (mkServiceName raw)
                -- Priority defaults to 100 when absent (backwards compatibility).
                p <- fromMaybe 100 <$> o .:? "priority"
                priority <- either (fail . show) pure (mkPriority p)
                pure (EnableWithPriority name priority)
            "service_disable" -> do
                raw <- o .: "name"
                name <- either (fail . show) pure (mkServiceName raw)
                -- Priority defaults to 100 when absent (backwards compatibility).
                p <- fromMaybe 100 <$> o .:? "priority"
                priority <- either (fail . show) pure (mkPriority p)
                pure (DisableWithPriority name priority)
            _unknownType -> fail $ "unknown service action: " <> T.unpack t
