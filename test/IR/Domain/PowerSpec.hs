module IR.Domain.PowerSpec (tests) where

import IR.Domain.Power
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "IR.Domain.Power"
        [ testCase "Performance variant" $ Performance @?= Performance
        , testCase "Balanced variant" $ Balanced @?= Balanced
        , testCase "PowerSave variant" $ PowerSave @?= PowerSave
        , testCase "SetPowerProfile carries profile" $
            SetPowerProfile Performance @?= SetPowerProfile Performance
        ]
