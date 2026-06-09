{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.ProcessSpec (tests) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import IR.Domain.Error
import IR.Domain.Process
import Test.Tasty
import Test.Tasty.HUnit

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
        ]
