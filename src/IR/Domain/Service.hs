{-# LANGUAGE OverloadedStrings #-}

-- | Service-related IR nodes: actions and base state for managing system services.
module IR.Domain.Service (
    ServiceName (..),
    mkServiceName,
    serviceNameText,
    Priority (..),
    mkPriority,
    priorityInt,
    Action (..),
    ServiceBaseState (..),
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (DomainError, mkName, mkPercent, renderDomainError)

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
                name <- either (fail . T.unpack . renderDomainError) pure (mkServiceName raw)
                -- Priority defaults to 100 when absent (backwards compatibility).
                p <- fromMaybe 100 <$> o .:? "priority"
                priority <- either (fail . T.unpack . renderDomainError) pure (mkPriority p)
                pure (EnableWithPriority name priority)
            "service_disable" -> do
                raw <- o .: "name"
                name <- either (fail . T.unpack . renderDomainError) pure (mkServiceName raw)
                -- Priority defaults to 100 when absent (backwards compatibility).
                p <- fromMaybe 100 <$> o .:? "priority"
                priority <- either (fail . T.unpack . renderDomainError) pure (mkPriority p)
                pure (DisableWithPriority name priority)
            _unknownType -> fail $ "unknown service action: " <> T.unpack t

{- | Base state of a managed service: the state the reconciler returns the
service to when no profile acts on it. Deliberately distinct from 'Action' —
"desired under condition" and "base" are different roles.
-}
data ServiceBaseState = Enabled | Disabled
    deriving (Eq, Show)

instance ToJSON ServiceBaseState where
    toJSON Enabled = "enabled"
    toJSON Disabled = "disabled"

instance FromJSON ServiceBaseState where
    parseJSON = withText "ServiceBaseState" $ \t -> case t of
        "enabled" -> pure Enabled
        "disabled" -> pure Disabled
        _unknownState -> fail $ "unknown service base state: " <> T.unpack t
