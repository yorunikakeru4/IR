{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Domain.ServiceSpec (tests) where

import Data.Aeson (decode, encode)
import IR.Arbitraries ()
import IR.Domain.Error (DomainError (..))
import IR.Domain.Service
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

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
                    "{\"type\":\"service_enable\",\"name\":\"foo\",\"priority\":200}" ::
                    Maybe Action
                )
                    @?= Nothing
            , testCase "absent priority field defaults to 100" $
                ( decode
                    "{\"type\":\"service_enable\",\"name\":\"sshd\"}" ::
                    Maybe Action
                )
                    @?= Just (EnableWithPriority (ServiceName "sshd") (unsafePriority 100))
            , testCase "absent priority on disable defaults to 100" $
                ( decode
                    "{\"type\":\"service_disable\",\"name\":\"docker\"}" ::
                    Maybe Action
                )
                    @?= Just (DisableWithPriority (ServiceName "docker") (unsafePriority 100))
            ]
        , testGroup
            "properties"
            [ testProperty "serviceNameText . mkServiceName roundtrips non-empty text" $
                \name ->
                    fmap serviceNameText (mkServiceName (serviceNameText name))
                        === Right (serviceNameText name)
            , testProperty "priorityInt . mkPriority roundtrips valid priority" $
                \p ->
                    fmap priorityInt (mkPriority (priorityInt p))
                        === Right (priorityInt p)
            , testProperty "mkPriority rejects out-of-range values" $
                \i ->
                    (i < 0 || i > 100) ==>
                        mkPriority i === Left (InvalidPercent i)
            , testProperty "JSON roundtrip for Action" $
                \(a :: Action) -> decode (encode a) === Just a
            ]
        , testGroup
            "ServiceBaseState JSON"
            [ testCase "encodes Enabled as \"enabled\"" $
                encode Enabled @?= "\"enabled\""
            , testCase "encodes Disabled as \"disabled\"" $
                encode Disabled @?= "\"disabled\""
            , testCase "decodes \"enabled\" to Enabled" $
                decode "\"enabled\"" @?= Just Enabled
            , testCase "decodes \"disabled\" to Disabled" $
                decode "\"disabled\"" @?= Just Disabled
            , testCase "rejects unknown base state" $
                (decode "\"sometimes\"" :: Maybe ServiceBaseState) @?= Nothing
            , testCase "round-trips Enabled" $
                decode (encode Enabled) @?= Just Enabled
            , testCase "round-trips Disabled" $
                decode (encode Disabled) @?= Just Disabled
            ]
        ]

{- | Test helper: construct a Priority without the Either wrapper.
Only for use in tests where the value is known-valid.
-}
unsafePriority :: Int -> Priority
unsafePriority p = case mkPriority p of
    Right priority -> priority
    Left err -> error $ "unsafePriority: " <> show err
