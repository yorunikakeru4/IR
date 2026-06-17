{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Domain.SystemSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import IR.Arbitraries ()
import IR.Domain.Error
import IR.Domain.System
import IR.ObserveStrategy (ObserveStrategy (..), mkIntervalMs)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "IR.Domain.System"
        [ testCase "accepts CPU threshold boundaries" $ do
            fmap cpuLoadThresholdValue (mkCpuLoadThreshold 0.0) @?= Right 0.0
            fmap cpuLoadThresholdValue (mkCpuLoadThreshold 1.0) @?= Right 1.0
        , testCase "rejects invalid and NaN CPU thresholds" $ do
            mkCpuLoadThreshold 1.1 @?= Left (InvalidThreshold 1.1)
            case mkCpuLoadThreshold (0 / 0) of
                Left (InvalidThreshold value) -> assertBool "expected NaN" (isNaN value)
                result -> assertFailure $ "unexpected result: " <> show result
        , testCase "accepts battery percentage boundaries" $ do
            fmap batteryPercentValue (mkBatteryPercent 0) @?= Right 0
            fmap batteryPercentValue (mkBatteryPercent 100) @?= Right 100
        , testCase "rejects invalid battery percentage" $
            mkBatteryPercent (-1) @?= Left (InvalidPercent (-1))
        , testCase "condition JSON roundtrip" $ do
            threshold <- expectRight (mkCpuLoadThreshold 0.7)
            let condition = CpuLoad threshold Nothing
            decode (encode condition) @?= Just condition
        , testCase "invalid condition JSON is rejected" $
            (decode (LBS.pack "{\"type\":\"cpu_load\",\"threshold\":2}") :: Maybe Condition)
                @?= Nothing
        , testCase "attachStrategy sets every missing system observation strategy" $ do
            threshold <- expectRight (mkCpuLoadThreshold 0.7)
            percent <- expectRight (mkBatteryPercent 20)
            strategy <- expectRight (Poll <$> mkIntervalMs 500)
            attachStrategy strategy (CpuLoad threshold Nothing)
                @?= Right (CpuLoad threshold (Just strategy))
            attachStrategy strategy (BatteryBelow percent Nothing)
                @?= Right (BatteryBelow percent (Just strategy))
            attachStrategy strategy (BatteryAbove percent Nothing)
                @?= Right (BatteryAbove percent (Just strategy))
        , testCase "attachStrategy rejects every existing system observation strategy" $ do
            threshold <- expectRight (mkCpuLoadThreshold 0.7)
            percent <- expectRight (mkBatteryPercent 20)
            oldStrategy <- expectRight (Poll <$> mkIntervalMs 250)
            newStrategy <- expectRight (Poll <$> mkIntervalMs 500)
            attachStrategy newStrategy (CpuLoad threshold (Just oldStrategy))
                @?= Left StrategyConflict
            attachStrategy newStrategy (BatteryBelow percent (Just oldStrategy))
                @?= Left StrategyConflict
            attachStrategy newStrategy (BatteryAbove percent (Just oldStrategy))
                @?= Left StrategyConflict
        , testGroup
            "properties"
            [ testProperty "cpuLoadThresholdValue . mkCpuLoadThreshold roundtrips valid threshold" $
                \t ->
                    fmap cpuLoadThresholdValue (mkCpuLoadThreshold (cpuLoadThresholdValue t))
                        === Right (cpuLoadThresholdValue t)
            , testProperty "batteryPercentValue . mkBatteryPercent roundtrips valid percent" $
                \p ->
                    fmap batteryPercentValue (mkBatteryPercent (batteryPercentValue p))
                        === Right (batteryPercentValue p)
            , testProperty "attachStrategy succeeds on every strategy-free system condition" $
                \t p s ->
                    attachStrategy s (CpuLoad t Nothing) === Right (CpuLoad t (Just s))
                        .&&. attachStrategy s (BatteryBelow p Nothing) === Right (BatteryBelow p (Just s))
                        .&&. attachStrategy s (BatteryAbove p Nothing) === Right (BatteryAbove p (Just s))
            , testProperty "attachStrategy rejects every system condition with existing strategy" $
                \old new t p ->
                    attachStrategy new (CpuLoad t (Just old)) === Left StrategyConflict
                        .&&. attachStrategy new (BatteryBelow p (Just old)) === Left StrategyConflict
                        .&&. attachStrategy new (BatteryAbove p (Just old)) === Left StrategyConflict
            , testProperty "JSON roundtrip for Condition" $
                \(c :: Condition) -> decode (encode c) === Just c
            ]
        ]

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight (Right value) = pure value
expectRight (Left err) = assertFailure ("expected Right, got Left: " <> show err)
