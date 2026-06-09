{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.SystemSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import IR.Domain.Error
import IR.Domain.System
import Test.Tasty
import Test.Tasty.HUnit

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
        ]

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight (Right value) = pure value
expectRight (Left err) = assertFailure ("expected Right, got Left: " <> show err)
