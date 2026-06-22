{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitraries () where

import qualified Data.Text as T
import qualified Domain.Network as Network
import qualified Domain.Package as Package
import qualified Domain.Power as Power
import qualified Domain.Process as Process
import qualified Domain.System as System
import qualified Domain.User as User
import ObserveStrategy (IntervalMs, ObserveStrategy (..), intervalMilliseconds, mkIntervalMs)
import Test.QuickCheck
import Types (ProfileName, mkProfileName, profileNameText)

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

instance Arbitrary Network.Port where
    arbitrary = Network.Port <$> arbitrary
    shrink (Network.Port p) = Network.Port <$> shrink p

instance Arbitrary Network.Resource where
    arbitrary = Network.PortResource <$> arbitrary
    shrink (Network.PortResource p) = Network.PortResource <$> shrink p

-- Policy constructors are not exported; smart constructors + listOf1 guarantee validity.
instance Arbitrary Network.Policy where
    arbitrary = do
        primary <- arbitrary
        alts <- listOf1 arbitrary
        pure (requireRight (Network.fallbackPolicy primary alts))
    shrink _ = []

instance Arbitrary Power.PowerProfile where
    arbitrary = oneof [pure Power.Performance, pure Power.Balanced, pure Power.PowerSave]
    shrink _ = []

instance Arbitrary Power.Action where
    arbitrary = Power.SetPowerProfile <$> arbitrary <*> choose (0, 100)
    shrink (Power.SetPowerProfile p pri) =
        [Power.SetPowerProfile p' pri | p' <- shrink p]
            <> [Power.SetPowerProfile p pri' | pri' <- shrink pri, pri' >= 0]

instance Arbitrary ProfileName where
    arbitrary = requireRight . mkProfileName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (profileNameText name))
        , Right candidate <- [mkProfileName (T.pack str)]
        ]

instance Arbitrary User.UserName where
    arbitrary = requireRight . User.mkUserName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | str <- shrink (T.unpack (User.userNameText name))
        , Right candidate <- [User.mkUserName (T.pack str)]
        ]

instance Arbitrary User.UserConfig where
    arbitrary =
        User.UserConfig
            <$> arbitrary
            <*> arbitrary
            <*> (T.pack <$> arbitrary)
            <*> listOf (T.pack <$> listOf1 arbitrary)
            <*> arbitrary
    shrink cfg =
        [cfg{User.userName = n} | n <- shrink (User.userName cfg)]
            <> [cfg{User.packages = ps} | ps <- shrink (User.packages cfg)]
