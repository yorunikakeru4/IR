{-# LANGUAGE OverloadedStrings #-}

module Domain.ErrorSpec (tests) where

import Domain.Error (DomainError (..), renderDomainError)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Domain.Error"
        [ testGroup
            "renderDomainError"
            [ testCase "EmptyName" $
                renderDomainError EmptyName @?= "name must not be empty"
            , testCase "EmptyList" $
                renderDomainError (EmptyList "actions") @?= "actions list must not be empty"
            , testCase "InvalidInterval" $
                renderDomainError (InvalidInterval 0) @?= "interval must be positive, got 0 ms"
            , testCase "InvalidPort" $
                renderDomainError (InvalidPort 70000) @?= "port must be between 0 and 65535, got 70000"
            , testCase "InvalidThreshold" $
                renderDomainError (InvalidThreshold 1.5) @?= "threshold must be between 0.0 and 1.0, got 1.5"
            , testCase "InvalidPercent" $
                renderDomainError (InvalidPercent 101) @?= "percent must be between 0 and 100, got 101"
            , testCase "StrategyConflict" $
                renderDomainError StrategyConflict @?= "observation strategy was already set"
            ]
        ]
