{-# LANGUAGE OverloadedStrings #-}

module Domain.ModuleSpec (tests) where

import Data.Aeson (ToJSON, Value (..), decode, encode, object, (.=))
import qualified Data.Text as Data.Text.Internal
import Domain.Error (DomainError (..))
import Domain.Module
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Domain.Module"
        [ testGroup
            "mkModuleName"
            [ testCase "rejects empty name" $
                mkModuleName "" @?= Left EmptyName
            , testCase "accepts non-empty name" $
                fmap moduleNameText (mkModuleName "main") @?= Right "main"
            ]
        , testGroup
            "ModuleDomain JSON"
            [ testCase "encodes PostgreSQLDomain as \"postgresql\"" $
                encode PostgreSQLDomain @?= "\"postgresql\""
            , testCase "encodes NginxDomain as \"nginx\"" $
                encode NginxDomain @?= "\"nginx\""
            , testCase "encodes ForgejoDomain as \"forgejo\"" $
                encode ForgejoDomain @?= "\"forgejo\""
            , testCase "decodes \"postgresql\" to PostgreSQLDomain" $
                decode "\"postgresql\"" @?= Just PostgreSQLDomain
            , testCase "rejects unknown domain" $
                (decode "\"redis\"" :: Maybe ModuleDomain) @?= Nothing
            ]
        , testGroup
            "ModuleRef JSON round-trip"
            [ testCase "round-trips postgresql/main" $
                let ref = ModuleRef PostgreSQLDomain (unsafeName "main")
                 in decode (encode ref) @?= Just ref
            , testCase "rejects empty name in ModuleRef JSON" $
                (decode "{\"domain\":\"nginx\",\"name\":\"\"}" :: Maybe ModuleRef) @?= Nothing
            ]
        , testGroup
            "LifecycleAction JSON"
            [ testCase "encodes EnableModule with correct type string" $
                assertEncodesTo
                    (EnableModule (ModuleRef NginxDomain (unsafeName "nginx")) 100)
                    ( object
                        [ "type" .= String "module_enable"
                        , "module" .= ModuleRef NginxDomain (unsafeName "nginx")
                        , "priority" .= (100 :: Int)
                        ]
                    )
            , testCase "EnableModule encodes priority field" $
                assertEncodesTo
                    (EnableModule (ModuleRef NginxDomain (unsafeName "nginx")) 80)
                    ( object
                        [ "type" .= String "module_enable"
                        , "module" .= ModuleRef NginxDomain (unsafeName "nginx")
                        , "priority" .= (80 :: Int)
                        ]
                    )
            , testCase "round-trips EnableModule with priority" $
                let a = EnableModule (ModuleRef NginxDomain (unsafeName "nginx")) 80
                 in decode (encode a) @?= Just a
            , testCase "round-trips DisableModule with priority" $
                let a = DisableModule (ModuleRef PostgreSQLDomain (unsafeName "main")) 100
                 in decode (encode a) @?= Just a
            , testCase "round-trips RestartModule with priority" $
                let a = RestartModule (ModuleRef NginxDomain (unsafeName "web")) 80
                 in decode (encode a) @?= Just a
            , testCase "decodes RestartModule without priority field defaults to 100" $
                decode
                    "{\"type\":\"module_restart\",\"module\":{\"domain\":\"nginx\",\"name\":\"web\"}}"
                    @?= Just (RestartModule (ModuleRef NginxDomain (unsafeName "web")) 100)
            , testCase "decodes EnableModule without priority field defaults to 100" $
                decode
                    "{\"type\":\"module_enable\",\"module\":{\"domain\":\"nginx\",\"name\":\"nginx\"}}"
                    @?= Just (EnableModule (ModuleRef NginxDomain (unsafeName "nginx")) 100)
            , testCase "rejects unknown type" $
                ( decode
                    "{\"type\":\"module_pause\",\"module\":{\"domain\":\"nginx\",\"name\":\"nginx\"}}" ::
                    Maybe LifecycleAction
                )
                    @?= Nothing
            ]
        , testGroup
            "InstallAction JSON"
            [ testCase "encodes type as module_install" $
                assertEncodesTo
                    (InstallModule (ModuleRef ForgejoDomain (unsafeName "forgejo")))
                    ( object
                        [ "type" .= String "module_install"
                        , "module" .= ModuleRef ForgejoDomain (unsafeName "forgejo")
                        ]
                    )
            , testCase "round-trips InstallModule" $
                let a = InstallModule (ModuleRef ForgejoDomain (unsafeName "forgejo"))
                 in decode (encode a) @?= Just a
            , testCase "rejects unknown type in InstallAction" $
                (decode "{\"type\":\"module_install_bad\",\"module\":{\"domain\":\"forgejo\",\"name\":\"forgejo\"}}" :: Maybe InstallAction)
                    @?= Nothing
            ]
        , testGroup
            "ConfigureAction JSON"
            [ testCase "encodes type as module_configure" $
                let v = String "test"
                    a = ConfigureModule (ModuleRef ForgejoDomain (unsafeName "forgejo")) "abc123" v
                 in assertEncodesTo
                        a
                        ( object
                            [ "type" .= String "module_configure"
                            , "module" .= ModuleRef ForgejoDomain (unsafeName "forgejo")
                            , "config_hash" .= String "abc123"
                            , "config_json" .= v
                            ]
                        )
            , testCase "round-trips ConfigureModule" $
                let v = object ["http_port" .= (3000 :: Int)]
                    a = ConfigureModule (ModuleRef ForgejoDomain (unsafeName "forgejo")) "deadbeef" v
                 in decode (encode a) @?= Just a
            , testCase "encodes config_hash field" $
                let v = String "x"
                    a = ConfigureModule (ModuleRef ForgejoDomain (unsafeName "forgejo")) "myhash" v
                 in assertEncodesTo
                        a
                        ( object
                            [ "type" .= String "module_configure"
                            , "module" .= ModuleRef ForgejoDomain (unsafeName "forgejo")
                            , "config_hash" .= String "myhash"
                            , "config_json" .= v
                            ]
                        )
            , testCase "rejects unknown type in ConfigureAction" $
                ( decode
                    "{\"type\":\"module_configure_bad\",\"module\":{\"domain\":\"forgejo\",\"name\":\"forgejo\"},\"config_hash\":\"hash\",\"config_json\":{}}" ::
                    Maybe ConfigureAction
                )
                    @?= Nothing
            ]
        , testGroup
            "UnconfigureAction JSON"
            [ testCase "encodes type as module_unconfigure" $
                assertEncodesTo
                    (UnconfigureModule (ModuleRef NginxDomain (unsafeName "nginx")))
                    ( object
                        [ "type" .= String "module_unconfigure"
                        , "module" .= ModuleRef NginxDomain (unsafeName "nginx")
                        ]
                    )
            , testCase "round-trips UnconfigureModule" $
                let a = UnconfigureModule (ModuleRef NginxDomain (unsafeName "nginx"))
                 in decode (encode a) @?= Just a
            , testCase "rejects unknown type in UnconfigureAction" $
                (decode "{\"type\":\"module_unconfigure_bad\",\"module\":{\"domain\":\"nginx\",\"name\":\"nginx\"}}" :: Maybe UnconfigureAction)
                    @?= Nothing
            ]
        ]

assertEncodesTo :: (ToJSON a) => a -> Value -> Assertion
assertEncodesTo actual expected =
    decode (encode actual) @?= Just expected

unsafeName :: Data.Text.Internal.Text -> ModuleName
unsafeName t =
    case mkModuleName t of
        Right n -> n
        Left _ -> error "unsafeName: empty string"
