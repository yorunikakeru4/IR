{-# LANGUAGE OverloadedStrings #-}

-- | Network-related IR nodes: port allocation and fallback policies.
module Domain.Network (
    Port (..),
    Resource (..),
    Policy,
    fallbackPolicy,
    ports,
    NetworkConfig (..),
    emptyNetworkConfig,
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)
import Domain.Error (DomainError (EmptyList), parseDomain)

-- | A TCP/UDP port number (0–65535).
newtype Port = Port Word16
    deriving (Eq, Show)

-- | A network resource that can serve as a fallback target.
data Resource
    = PortResource Port
    deriving (Eq, Show)

-- | Network-domain policies for port allocation.
data Policy
    = -- | Prefer the primary resource; fall back to alternatives if unavailable.
      Fallback Resource [Resource]
    deriving (Eq, Show)

fallbackPolicy :: Resource -> [Resource] -> Either DomainError Policy
fallbackPolicy _primary [] = Left (EmptyList "FallbackCandidates")
fallbackPolicy primary alts = Right (Fallback primary alts)

-- | Global network configuration: ports explicitly permitted through the firewall.
data NetworkConfig = NetworkConfig
    { networkAllowPorts :: [Port]
    }
    deriving (Eq, Show)

emptyNetworkConfig :: NetworkConfig
emptyNetworkConfig = NetworkConfig []

instance Semigroup NetworkConfig where
    NetworkConfig a <> NetworkConfig b = NetworkConfig (a <> b)

instance Monoid NetworkConfig where
    mempty = emptyNetworkConfig

-- | Convert a list of **raw** port numbers to 'Port' values.
ports :: [Word16] -> [Port]
ports = map Port

instance ToJSON Port where
    toJSON (Port p) = toJSON p

instance FromJSON Port where
    parseJSON = fmap Port . parseJSON

instance ToJSON Resource where
    toJSON (PortResource (Port p)) =
        object ["type" .= ("port" :: Text), "value" .= p]

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "port" -> PortResource . Port <$> o .: "value"
            _unknownType -> fail $ "unknown resource type: " <> T.unpack t

instance ToJSON Policy where
    toJSON (Fallback primary alts) =
        object
            [ "type" .= ("fallback" :: Text)
            , "primary" .= primary
            , "alternatives" .= alts
            ]

instance FromJSON Policy where
    parseJSON = withObject "Network.Policy" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "fallback" -> do
                primary <- o .: "primary"
                alts <- o .: "alternatives"
                parseDomain (fallbackPolicy primary alts)
            _unknownType -> fail $ "unknown network policy: " <> T.unpack t

instance ToJSON NetworkConfig where
    toJSON (NetworkConfig ps) = object ["allow_ports" .= ps]

instance FromJSON NetworkConfig where
    parseJSON = withObject "NetworkConfig" $ \o ->
        NetworkConfig <$> o .: "allow_ports"
