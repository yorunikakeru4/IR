{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.ServiceSpec (tests) where

import IR.Domain.Service
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "IR.Domain.Service"
        [ testCase "ServiceName wraps text" $
            let ServiceName n = ServiceName "docker" in n @?= "docker"
        , testCase "Disable carries name" $
            Disable (ServiceName "docker") @?= Disable (ServiceName "docker")
        , testCase "Enable carries name" $
            Enable (ServiceName "sshd") @?= Enable (ServiceName "sshd")
        ]
