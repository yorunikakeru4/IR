{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.PowerSpec (tests) where

import Arbitraries ()
import Data.Aeson (decode, encode)
import Domain.Power
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "Domain.Power"
        [ testCase "Performance variant" $ Performance @?= Performance
        , testCase "Balanced variant" $ Balanced @?= Balanced
        , testCase "PowerSave variant" $ PowerSave @?= PowerSave
        , testCase "SetPowerProfile carries profile and priority" $
            SetPowerProfile Performance 100 @?= SetPowerProfile Performance 100
        , testCase "decodes SetPowerProfile without priority field defaults to 100" $
            (decode "{\"type\":\"power_profile\",\"value\":\"performance\"}" :: Maybe Action)
                @?= Just (SetPowerProfile Performance 100)
        , testGroup
            "properties"
            [ testProperty "parsePowerProfile . renderPowerProfile roundtrips any profile" $
                \p -> parsePowerProfile (renderPowerProfile p) === Right p
            , testProperty "JSON roundtrip for Action" $
                \(a :: Action) -> decode (encode a) === Just a
            ]
        ]
