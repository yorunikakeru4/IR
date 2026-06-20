{-# LANGUAGE OverloadedStrings #-}

-- | Closed root sum of all executable actions across domains.
module Action (
    Action (..),
    LiftAction (..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Domain.Module as Module
import qualified Domain.Power as Power

{- | A desired system state change to be applied by the Executor.
Adding a new action domain requires extending this sum and bumping 'Types.IRVersion'.
-}
data Action
    = PowerAction Power.Action
    | ModuleAction Module.LifecycleAction
    deriving (Eq, Show)

{- | Lift a domain action into the root 'Action' sum.
Implement one instance per domain when adding a new 'Action' constructor.
-}
class LiftAction a where
    liftAction :: a -> Action

instance LiftAction Power.Action where
    liftAction = PowerAction

instance LiftAction Module.LifecycleAction where
    liftAction = ModuleAction

instance ToJSON Action where
    toJSON (PowerAction a) = toJSON a
    toJSON (ModuleAction a) = toJSON a

instance FromJSON Action where
    parseJSON v =
        withObject
            "Action"
            ( \o -> do
                t <- o .: "type" :: Parser Text
                case t of
                    "power_profile" -> PowerAction <$> parseJSON v
                    "module_enable" -> ModuleAction <$> parseJSON v
                    "module_disable" -> ModuleAction <$> parseJSON v
                    "module_restart" -> ModuleAction <$> parseJSON v
                    _unknownType -> fail $ "unknown action type: " <> T.unpack t
            )
            v
