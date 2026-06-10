{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.ServiceSpec (tests) where

import Data.Aeson (decode, encode)
import IR.Domain.Error (DomainError (..))
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
        , testGroup
            "mkPriority"
            [ testCase "accepts 0" $
                fmap priorityInt (mkPriority 0) @?= Right 0
            , testCase "accepts 100" $
                fmap priorityInt (mkPriority 100) @?= Right 100
            , testCase "accepts 50" $
                fmap priorityInt (mkPriority 50) @?= Right 50
            , testCase "rejects -1" $
                mkPriority (-1) @?= Left (InvalidPercent (-1))
            , testCase "rejects 101" $
                mkPriority 101 @?= Left (InvalidPercent 101)
            ]
        , testGroup
            "EnableWithPriority / DisableWithPriority"
            [ testCase "round-trips EnableWithPriority via JSON" $
                case mkPriority 10 of
                    Left err -> assertFailure $ show err
                    Right p ->
                        let action = EnableWithPriority (ServiceName "foo") p
                         in (decode (encode action) :: Maybe Action) @?= Just action
            , testCase "round-trips DisableWithPriority via JSON" $
                case mkPriority 5 of
                    Left err -> assertFailure $ show err
                    Right p ->
                        let action = DisableWithPriority (ServiceName "bar") p
                         in (decode (encode action) :: Maybe Action) @?= Just action
            , testCase "rejects out-of-range priority in JSON" $
                ( decode
                    "{\"type\":\"service_enable_priority\",\"name\":\"foo\",\"priority\":200}" ::
                    Maybe Action
                )
                    @?= Nothing
            ]
        , testGroup
            "legacy JSON aliases"
            [ testCase "service_enable decodes as priority 100" $
                ( decode
                    "{\"type\":\"service_enable\",\"name\":\"sshd\"}" ::
                    Maybe Action
                )
                    @?= Just (EnableWithPriority (ServiceName "sshd") (unsafePriority 100))
            , testCase "service_disable decodes as priority 100" $
                ( decode
                    "{\"type\":\"service_disable\",\"name\":\"docker\"}" ::
                    Maybe Action
                )
                    @?= Just (DisableWithPriority (ServiceName "docker") (unsafePriority 100))
            ]
        ]

{- | Test helper: construct a Priority without the Either wrapper.
Only for use in tests where the value is known-valid.
-}
unsafePriority :: Int -> Priority
unsafePriority p = case mkPriority p of
    Right priority -> priority
    Left err -> error $ "unsafePriority: " <> show err
