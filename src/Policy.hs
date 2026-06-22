{-# LANGUAGE OverloadedStrings #-}

-- | Closed root sum of all declarative constraints across domains.
module Policy (
    Policy (..),
    LiftPolicy (..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Domain.Network as Network

{- | A declarative constraint or decision space delegated to the Executor.
Unlike 'Action.Action', a policy does not prescribe the exact outcome —
the Executor resolves it against observed runtime state.
-}
data Policy
    = NetworkPolicy Network.Policy
    deriving (Eq, Show)

{- | Lift a domain policy into the root 'Policy' sum.
Implement one instance per domain when adding a new 'Policy' constructor.
-}
class LiftPolicy a where
    liftPolicy :: a -> Policy

instance LiftPolicy Network.Policy where
    liftPolicy = NetworkPolicy

instance ToJSON Policy where
    toJSON (NetworkPolicy p) = toJSON p

instance FromJSON Policy where
    parseJSON v =
        withObject
            "Policy"
            ( \o -> do
                t <- o .: "type" :: Parser Text
                case t of
                    "fallback" -> NetworkPolicy <$> parseJSON v
                    _unknownType -> fail $ "unknown policy type: " <> T.unpack t
            )
            v
