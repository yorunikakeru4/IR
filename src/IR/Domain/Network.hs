{-# LANGUAGE OverloadedStrings #-}

-- | Network-related IR nodes: port allocation policies.
module IR.Domain.Network (
    Port (..),
    Resource (..),
    Policy,
    allowPortsPolicy,
    fallbackPolicy,
    ports,
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)
import IR.Domain.Error (DomainError (EmptyList))

-- | A TCP/UDP port number (0–65535).
newtype Port = Port Word16
    deriving (Eq, Show)

-- | A network resource that can serve as a fallback target.
data Resource
    = PortResource Port
    deriving (Eq, Show)

-- | Network-domain policies for port allocation.
data Policy
    = -- | Declare that a set of ports is permitted for the service.
      AllowPorts [Port]
    | -- | Prefer the primary resource; fall back to alternatives if unavailable.
      Fallback Resource [Resource]
    deriving (Eq, Show)

allowPortsPolicy :: [Port] -> Either DomainError Policy
allowPortsPolicy [] = Left (EmptyList "AllowPorts")
allowPortsPolicy ps = Right (AllowPorts ps)

fallbackPolicy :: Resource -> [Resource] -> Either DomainError Policy
fallbackPolicy _primary [] = Left (EmptyList "FallbackCandidates")
fallbackPolicy primary alts = Right (Fallback primary alts)

-- | Convert a list of **raw** port numbers to 'Port' values. This is a convenience function for constructing 'Policy' values from raw data.
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
    toJSON (AllowPorts ps) =
        object ["type" .= ("allow_ports" :: Text), "values" .= ps]
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
            "allow_ports" -> do
                ps <- o .: "values"
                either (fail . show) pure (allowPortsPolicy ps)
            "fallback" -> do
                primary <- o .: "primary"
                alts <- o .: "alternatives"
                either (fail . show) pure (fallbackPolicy primary alts)
            _unknownType -> fail $ "unknown network policy: " <> T.unpack t
