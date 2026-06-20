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
        , testCase "SetPowerProfile carries profile" $
            SetPowerProfile Performance @?= SetPowerProfile Performance
        , testGroup
            "properties"
            [ testProperty "parsePowerProfile . renderPowerProfile roundtrips any profile" $
                \p -> parsePowerProfile (renderPowerProfile p) === Right p
            , testProperty "JSON roundtrip for Action" $
                \(a :: Action) -> decode (encode a) === Just a
            ]
        ]
