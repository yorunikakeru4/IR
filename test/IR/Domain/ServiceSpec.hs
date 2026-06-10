{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.ServiceSpec (tests) where

import Data.Aeson (decode, encode)
import IR.Domain.Error (DomainError (EmptyName))
import IR.Domain.Service
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "IR.Domain.Service"
        [ testGroup
            "mkServiceName"
            [ testCase "rejects empty name" $
                mkServiceName "" @?= Left EmptyName
            , testCase "accepts non-empty name" $
                fmap serviceNameText (mkServiceName "docker") @?= Right "docker"
            ]
        , testCase "Disable carries name" $
            Disable (ServiceName "docker") @?= Disable (ServiceName "docker")
        , testCase "Enable carries name" $
            Enable (ServiceName "sshd") @?= Enable (ServiceName "sshd")
        , testGroup
            "FromJSON"
            [ testCase "rejects empty service name in service_disable" $
                (decode "{\"type\":\"service_disable\",\"name\":\"\"}" :: Maybe Action)
                    @?= Nothing
            , testCase "rejects empty service name in service_enable" $
                (decode "{\"type\":\"service_enable\",\"name\":\"\"}" :: Maybe Action)
                    @?= Nothing
            , testCase "round-trips non-empty disable" $
                let action = Disable (ServiceName "docker")
                 in (decode (encode action) :: Maybe Action) @?= Just action
            , testCase "round-trips non-empty enable" $
                let action = Enable (ServiceName "sshd")
                 in (decode (encode action) :: Maybe Action) @?= Just action
            ]
        ]
