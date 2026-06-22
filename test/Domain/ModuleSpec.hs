{-# LANGUAGE OverloadedStrings #-}

module Domain.ModuleSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isInfixOf)
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
                assertBool "missing module_enable" $
                    "module_enable" `isInfixOf` BSLC.unpack (encode (EnableModule (ModuleRef NginxDomain (unsafeName "nginx")) 100))
            , testCase "EnableModule encodes priority field" $
                assertBool "missing priority" $
                    "\"priority\"" `isInfixOf` BSLC.unpack (encode (EnableModule (ModuleRef NginxDomain (unsafeName "nginx")) 80))
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
        ]

unsafeName :: Data.Text.Internal.Text -> ModuleName
unsafeName t =
    case mkModuleName t of
        Right n -> n
        Left _ -> error "unsafeName: empty string"
