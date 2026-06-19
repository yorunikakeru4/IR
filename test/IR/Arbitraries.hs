{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Arbitraries () where

import qualified Data.Text as T
import qualified IR.Domain.Network as Network
import qualified IR.Domain.Package as Package
import qualified IR.Domain.Power as Power
import qualified IR.Domain.Process as Process
import qualified IR.Domain.Service as Service
import qualified IR.Domain.System as System
import IR.ObserveStrategy (IntervalMs, ObserveStrategy (..), intervalMilliseconds, mkIntervalMs)
import IR.Types (ProfileName, ServiceSectionName, mkProfileName, mkServiceSectionName, profileNameText, serviceSectionNameText)
import Test.QuickCheck

requireRight :: (Show e) => Either e a -> a
requireRight (Right x) = x
requireRight (Left e) = error ("Arbitraries: unexpected Left: " <> show e)

instance Arbitrary Package.PackageName where
    arbitrary = requireRight . Package.mkPackageName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (Package.packageNameText name))
        , Right candidate <- [Package.mkPackageName (T.pack str)]
        ]

instance Arbitrary Process.ProcessName where
    arbitrary = requireRight . Process.mkProcessName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (Process.processNameText name))
        , Right candidate <- [Process.mkProcessName (T.pack str)]
        ]

instance Arbitrary Process.AppName where
    arbitrary = requireRight . Process.mkAppName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (Process.appNameText name))
        , Right candidate <- [Process.mkAppName (T.pack str)]
        ]

instance Arbitrary IntervalMs where
    arbitrary = requireRight . mkIntervalMs . getPositive <$> arbitrary
    shrink ms =
        [ candidate
        | i <- shrink (intervalMilliseconds ms)
        , Right candidate <- [mkIntervalMs i]
        ]

instance Arbitrary ObserveStrategy where
    arbitrary = Poll <$> arbitrary
    shrink (Poll ms) = Poll <$> shrink ms

instance Arbitrary System.CpuLoadThreshold where
    arbitrary = requireRight . System.mkCpuLoadThreshold <$> choose (0.0, 1.0)
    shrink t =
        [ candidate
        | v <- shrink (System.cpuLoadThresholdValue t)
        , Right candidate <- [System.mkCpuLoadThreshold v]
        ]

instance Arbitrary System.BatteryPercent where
    arbitrary = requireRight . System.mkBatteryPercent <$> choose (0, 100)
    shrink p =
        [ candidate
        | v <- shrink (System.batteryPercentValue p)
        , Right candidate <- [System.mkBatteryPercent v]
        ]

instance Arbitrary Process.Condition where
    arbitrary =
        oneof
            [ Process.ProcessRunning <$> arbitrary <*> arbitrary
            , Process.AppRunning <$> arbitrary <*> arbitrary
            ]
    shrink (Process.ProcessRunning name obs) =
        [Process.ProcessRunning name' obs | name' <- shrink name]
            <> [Process.ProcessRunning name obs' | obs' <- shrink obs]
    shrink (Process.AppRunning name obs) =
        [Process.AppRunning name' obs | name' <- shrink name]
            <> [Process.AppRunning name obs' | obs' <- shrink obs]

instance Arbitrary System.Condition where
    arbitrary =
        oneof
            [ System.CpuLoad <$> arbitrary <*> arbitrary
            , System.BatteryBelow <$> arbitrary <*> arbitrary
            , System.BatteryAbove <$> arbitrary <*> arbitrary
            ]
    shrink (System.CpuLoad t obs) =
        [System.CpuLoad t' obs | t' <- shrink t]
            <> [System.CpuLoad t obs' | obs' <- shrink obs]
    shrink (System.BatteryBelow p obs) =
        [System.BatteryBelow p' obs | p' <- shrink p]
            <> [System.BatteryBelow p obs' | obs' <- shrink obs]
    shrink (System.BatteryAbove p obs) =
        [System.BatteryAbove p' obs | p' <- shrink p]
            <> [System.BatteryAbove p obs' | obs' <- shrink obs]

instance Arbitrary Service.ServiceName where
    arbitrary = requireRight . Service.mkServiceName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (Service.serviceNameText name))
        , Right candidate <- [Service.mkServiceName (T.pack str)]
        ]

instance Arbitrary Service.Priority where
    arbitrary = requireRight . Service.mkPriority <$> choose (0, 100)
    shrink p =
        [ candidate
        | v <- shrink (Service.priorityInt p)
        , Right candidate <- [Service.mkPriority v]
        ]

instance Arbitrary Service.ServiceBaseState where
    arbitrary = oneof [pure Service.Enabled, pure Service.Disabled]
    shrink _ = []

instance Arbitrary Service.Action where
    arbitrary =
        oneof
            [ Service.EnableWithPriority <$> arbitrary <*> arbitrary
            , Service.DisableWithPriority <$> arbitrary <*> arbitrary
            ]
    shrink (Service.EnableWithPriority n p) =
        [Service.EnableWithPriority n' p | n' <- shrink n]
            <> [Service.EnableWithPriority n p' | p' <- shrink p]
    shrink (Service.DisableWithPriority n p) =
        [Service.DisableWithPriority n' p | n' <- shrink n]
            <> [Service.DisableWithPriority n p' | p' <- shrink p]

instance Arbitrary Network.Port where
    arbitrary = Network.Port <$> arbitrary
    shrink (Network.Port p) = Network.Port <$> shrink p

instance Arbitrary Network.Resource where
    arbitrary = Network.PortResource <$> arbitrary
    shrink (Network.PortResource p) = Network.PortResource <$> shrink p

-- Policy constructors are not exported; smart constructors + listOf1 guarantee validity.
instance Arbitrary Network.Policy where
    arbitrary =
        oneof
            [ requireRight . Network.allowPortsPolicy <$> listOf1 arbitrary
            , do
                primary <- arbitrary
                alts <- listOf1 arbitrary
                pure (requireRight (Network.fallbackPolicy primary alts))
            ]
    shrink _ = []

instance Arbitrary Power.PowerProfile where
    arbitrary = oneof [pure Power.Performance, pure Power.Balanced, pure Power.PowerSave]
    shrink _ = []

instance Arbitrary Power.Action where
    arbitrary = Power.SetPowerProfile <$> arbitrary
    shrink (Power.SetPowerProfile p) = Power.SetPowerProfile <$> shrink p

instance Arbitrary ProfileName where
    arbitrary = requireRight . mkProfileName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (profileNameText name))
        , Right candidate <- [mkProfileName (T.pack str)]
        ]

instance Arbitrary ServiceSectionName where
    arbitrary = requireRight . mkServiceSectionName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (serviceSectionNameText name))
        , Right candidate <- [mkServiceSectionName (T.pack str)]
        ]
