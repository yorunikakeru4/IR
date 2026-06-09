{-# LANGUAGE OverloadedStrings #-}

-- | Closed root sum of all declarative constraints across domains.
module IR.Policy (
    Policy (..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified IR.Domain.Network as Network

{- | A declarative constraint or decision space delegated to the Executor.
Unlike 'IR.Action.Action', a policy does not prescribe the exact outcome —
the Executor resolves it against observed runtime state.
-}
data Policy
    = NetworkPolicy Network.Policy
    deriving (Eq, Show)

instance ToJSON Policy where
    toJSON (NetworkPolicy p) = toJSON p

instance FromJSON Policy where
    parseJSON v =
        withObject
            "Policy"
            ( \o -> do
                t <- o .: "type" :: Parser Text
                case t of
                    "allow_ports" -> NetworkPolicy <$> parseJSON v
                    "fallback" -> NetworkPolicy <$> parseJSON v
                    _unknownType -> fail $ "unknown policy type: " <> T.unpack t
            )
            v
