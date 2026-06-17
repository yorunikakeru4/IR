{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Domain.NetworkSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isRight)
import IR.Arbitraries ()
import IR.Domain.Error
import IR.Domain.Network
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

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
        , testGroup
            "properties"
            [ testProperty "allowPortsPolicy accepts any non-empty port list" $
                \ps -> isRight (allowPortsPolicy (getNonEmpty ps))
            , testProperty "fallbackPolicy accepts any non-empty alternative list" $
                \primary alts -> isRight (fallbackPolicy primary (getNonEmpty alts))
            , testProperty "JSON roundtrip for Policy" $
                \(pol :: Policy) -> decode (encode pol) === Just pol
            ]
        ]

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight (Right value) = pure value
expectRight (Left err) = assertFailure ("expected Right, got Left: " <> show err)
