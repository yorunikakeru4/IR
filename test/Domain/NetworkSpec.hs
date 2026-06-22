{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.NetworkSpec (tests) where

import Arbitraries ()
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isRight)
import Domain.Error (DomainError (..))
import Domain.Network
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "Domain.Network"
        [ testCase "rejects empty fallback alternatives" $
            fallbackPolicy (PortResource (Port 80)) [] @?= Left (EmptyList "FallbackCandidates")
        , testCase "valid fallback JSON roundtrip" $ do
            policy <- expectRight (fallbackPolicy (PortResource (Port 80)) [PortResource (Port 8080)])
            decode (encode policy) @?= Just policy
        , testCase "fallback with empty alternatives is rejected from JSON" $
            (decode (LBS.pack "{\"type\":\"fallback\",\"primary\":{\"type\":\"port\",\"value\":80},\"alternatives\":[]}") :: Maybe Policy)
                @?= Nothing
        , testCase "NetworkConfig JSON roundtrip" $ do
            let cfg = NetworkConfig (ports [80, 443])
            decode (encode cfg) @?= Just cfg
        , testGroup
            "properties"
            [ testProperty "fallbackPolicy accepts any non-empty alternative list" $
                \primary alts -> isRight (fallbackPolicy primary (getNonEmpty alts))
            , testProperty "JSON roundtrip for Policy" $
                \(pol :: Policy) -> decode (encode pol) === Just pol
            ]
        ]

expectRight :: (Show errorValue) => Either errorValue value -> IO value
expectRight (Right value) = pure value
expectRight (Left err) = assertFailure ("expected Right, got Left: " <> show err)
