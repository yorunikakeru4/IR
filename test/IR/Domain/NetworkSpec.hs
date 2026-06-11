{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.NetworkSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import IR.Domain.Error
import IR.Domain.Network
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "IR.Domain.Network"
        [ testCase "rejects empty allow-ports list" $
            allowPortsPolicy [] @?= Left (EmptyList "AllowPorts")
        , testCase "rejects empty fallback alternatives" $
            fallbackPolicy (PortResource (Port 80)) [] @?= Left (EmptyList "FallbackCandidates")
        , testCase "valid allow-ports JSON roundtrip" $ do
            policy <- expectRight (allowPortsPolicy (ports [80, 443]))
            decode (encode policy) @?= Just policy
        , testCase "valid fallback JSON roundtrip" $ do
            policy <- expectRight (fallbackPolicy (PortResource (Port 80)) [PortResource (Port 8080)])
            decode (encode policy) @?= Just policy
        , testCase "invalid policy JSON is rejected" $
            (decode (LBS.pack "{\"type\":\"allow_ports\",\"values\":[]}") :: Maybe Policy)
                @?= Nothing
        , testCase "fallback with empty alternatives is rejected from JSON" $
            (decode (LBS.pack "{\"type\":\"fallback\",\"primary\":{\"type\":\"port\",\"value\":80},\"alternatives\":[]}") :: Maybe Policy)
                @?= Nothing
        ]

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight (Right value) = pure value
expectRight (Left err) = assertFailure ("expected Right, got Left: " <> show err)
