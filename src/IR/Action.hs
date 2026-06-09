{-# LANGUAGE OverloadedStrings #-}

-- | Closed root sum of all executable actions across domains.
module IR.Action (
    Action (..),
    LiftAction (..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified IR.Domain.Power as Power
import qualified IR.Domain.Service as Service

{- | A desired system state change to be applied by the Executor.
Adding a new action domain requires extending this sum and bumping 'IR.Types.IRVersion'.
-}
data Action
    = ServiceAction Service.Action
    | PowerAction Power.Action
    deriving (Eq, Show)

-- | Lift a domain action into the root 'Action' sum.
-- Implement one instance per domain when adding a new 'Action' constructor.
class LiftAction a where
    liftAction :: a -> Action

instance LiftAction Service.Action where
    liftAction = ServiceAction

instance LiftAction Power.Action where
    liftAction = PowerAction

instance ToJSON Action where
    toJSON (ServiceAction a) = toJSON a
    toJSON (PowerAction a) = toJSON a

instance FromJSON Action where
    parseJSON v =
        withObject
            "Action"
            ( \o -> do
                t <- o .: "type" :: Parser Text
                case t of
                    "service_disable" -> ServiceAction <$> parseJSON v
                    "service_enable" -> ServiceAction <$> parseJSON v
                    "power_profile" -> PowerAction <$> parseJSON v
                    _unknownType -> fail $ "unknown action type: " <> T.unpack t
            )
            v
