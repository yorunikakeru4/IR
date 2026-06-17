{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Domain.ProcessSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import IR.Arbitraries ()
import IR.Domain.Error
import IR.Domain.Process
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "IR.Domain.Process"
        [ testCase "rejects empty process name" $
            mkProcessName "" @?= Left EmptyName
        , testCase "accepts non-empty process name" $
            fmap processNameText (mkProcessName "steam") @?= Right "steam"
        , testCase "rejects non-positive polling interval" $ do
            mkIntervalMs 0 @?= Left (InvalidInterval 0)
            mkIntervalMs (-1) @?= Left (InvalidInterval (-1))
        , testCase "accepts positive polling interval" $
            fmap intervalMilliseconds (mkIntervalMs 500) @?= Right 500
        , testCase "invalid observation JSON is rejected" $
            (decode (LBS.pack "{\"type\":\"poll\",\"interval_ms\":0}") :: Maybe ObserveStrategy)
                @?= Nothing
        , testCase "empty process name JSON is rejected" $
            (decode (LBS.pack "{\"type\":\"process_running\",\"name\":\"\"}") :: Maybe Condition)
                @?= Nothing
        , testCase "attachStrategy sets a missing process observation strategy" $ do
            name <- expectRight (mkProcessName "steam")
            newStrategy <- expectRight (Poll <$> mkIntervalMs 500)
            attachStrategy newStrategy (ProcessRunning name Nothing)
                @?= Right (ProcessRunning name (Just newStrategy))
        , testCase "attachStrategy rejects an existing process observation strategy" $ do
            name <- expectRight (mkProcessName "steam")
            oldStrategy <- expectRight (Poll <$> mkIntervalMs 250)
            newStrategy <- expectRight (Poll <$> mkIntervalMs 500)
            attachStrategy newStrategy (ProcessRunning name (Just oldStrategy))
                @?= Left StrategyConflict
        , testGroup
            "properties"
            [ testProperty "processNameText . mkProcessName roundtrips non-empty text" $
                \name ->
                    fmap processNameText (mkProcessName (processNameText name))
                        === Right (processNameText name)
            , testProperty "intervalMilliseconds . mkIntervalMs roundtrips positive interval" $
                \ms ->
                    fmap intervalMilliseconds (mkIntervalMs (intervalMilliseconds ms))
                        === Right (intervalMilliseconds ms)
            , testProperty "mkIntervalMs rejects non-positive interval" $
                \i ->
                    i <= 0 ==>
                        mkIntervalMs i === Left (InvalidInterval i)
            , testProperty "attachStrategy sets strategy on strategy-free condition" $
                \name s ->
                    attachStrategy s (ProcessRunning name Nothing)
                        === Right (ProcessRunning name (Just s))
            , testProperty "attachStrategy rejects condition with existing strategy" $
                \name old new ->
                    attachStrategy new (ProcessRunning name (Just old))
                        === Left StrategyConflict
            , testProperty "JSON roundtrip for Condition" $
                \(c :: Condition) -> decode (encode c) === Just c
            ]
        ]

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight (Right value) = pure value
expectRight (Left err) = assertFailure ("expected Right, got Left: " <> show err)
