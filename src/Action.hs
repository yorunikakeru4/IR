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
    = DesiredAction DesiredAction
    | EffectAction EffectAction
    deriving (Eq, Show)

-- Action, whose state can be changed (should be with priority)
data DesiredAction
    = PowerAction Power.Action
    | ModuleAction Module.LifecycleAction
    deriving (Eq, Show)

-- Action, whose state can't be changed (no priority)
data EffectAction
    = ModuleInstallAction Module.InstallAction
    | ModuleConfigureAction Module.ConfigureAction
    | ModuleUnconfigureAction Module.UnconfigureAction
    deriving (Eq, Show)

{- | Lift a domain action into the root 'Action' sum.
Implement one instance per domain when adding a new 'Action' constructor.
-}
class LiftAction a where
    liftAction :: a -> Action

instance LiftAction Power.Action where
    liftAction = DesiredAction . PowerAction

instance LiftAction Module.LifecycleAction where
    liftAction = DesiredAction . ModuleAction

instance LiftAction Module.InstallAction where
    liftAction = EffectAction . ModuleInstallAction

instance LiftAction Module.ConfigureAction where
    liftAction = EffectAction . ModuleConfigureAction

instance LiftAction Module.UnconfigureAction where
    liftAction = EffectAction . ModuleUnconfigureAction

instance ToJSON Action where
    toJSON (DesiredAction a) = toJSON a
    toJSON (EffectAction a) = toJSON a

instance ToJSON DesiredAction where
    toJSON (PowerAction a) = toJSON a
    toJSON (ModuleAction a) = toJSON a

instance ToJSON EffectAction where
    toJSON (ModuleInstallAction a) = toJSON a
    toJSON (ModuleConfigureAction a) = toJSON a
    toJSON (ModuleUnconfigureAction a) = toJSON a

instance FromJSON Action where
    parseJSON v =
        withObject
            "Action"
            ( \o -> do
                t <- o .: "type" :: Parser Text
                case t of
                    "power_profile" ->
                        DesiredAction . PowerAction <$> parseJSON v
                    "module_enable" ->
                        DesiredAction . ModuleAction <$> parseJSON v
                    "module_disable" ->
                        DesiredAction . ModuleAction <$> parseJSON v
                    "module_restart" ->
                        DesiredAction . ModuleAction <$> parseJSON v
                    "module_install" ->
                        EffectAction . ModuleInstallAction <$> parseJSON v
                    "module_configure" ->
                        EffectAction . ModuleConfigureAction <$> parseJSON v
                    "module_unconfigure" ->
                        EffectAction . ModuleUnconfigureAction <$> parseJSON v
                    _ ->
                        fail $ "unknown action type: " <> T.unpack t
            )
            v
