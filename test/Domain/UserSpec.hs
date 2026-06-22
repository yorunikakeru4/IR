{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.UserSpec (tests) where

import Arbitraries ()
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isInfixOf)
import Domain.Error (DomainError (EmptyName))
import Domain.Package (mkPackageName)
import Domain.User
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "Domain.User"
        [ testGroup
            "UserName"
            [ testCase "rejects empty name" $
                mkUserName "" @?= Left EmptyName
            , testCase "accepts non-empty name" $
                fmap userNameText (mkUserName "alice") @?= Right "alice"
            , testCase "empty name JSON is rejected" $
                (decode "\"\"" :: Maybe UserName) @?= Nothing
            , testGroup
                "properties"
                [ testProperty "JSON roundtrip" $
                    \(name :: UserName) -> decode (encode name) === Just name
                ]
            ]
        , testGroup
            "UserConfig"
            [ testCase "round-trips full config" $
                let pkg = requireRight (mkPackageName "vim")
                    cfg =
                        UserConfig
                            { userName = requireRight (mkUserName "alice")
                            , isNormalUser = True
                            , description = "primary user"
                            , extraGroups = ["wheel", "audio"]
                            , packages = [pkg]
                            }
                 in decode (encode cfg) @?= Just cfg
            , testCase "round-trips minimal config" $
                let cfg =
                        UserConfig
                            { userName = requireRight (mkUserName "bob")
                            , isNormalUser = False
                            , description = ""
                            , extraGroups = []
                            , packages = []
                            }
                 in decode (encode cfg) @?= Just cfg
            , testCase "omits description when empty" $
                let cfg = emptyUserConfig
                 in assertBool "unexpected description key" $
                        not ("description" `isInfixOf` BSLC.unpack (encode cfg))
            , testCase "omits extra_groups when empty" $
                let cfg = emptyUserConfig
                 in assertBool "unexpected extra_groups key" $
                        not ("extra_groups" `isInfixOf` BSLC.unpack (encode cfg))
            , testCase "omits packages when empty" $
                let cfg = emptyUserConfig
                 in assertBool "unexpected packages key" $
                        not ("packages" `isInfixOf` BSLC.unpack (encode cfg))
            , testCase "normal_user defaults to false when absent" $
                let json = "{\"name\":\"alice\"}"
                 in fmap isNormalUser (decode json :: Maybe UserConfig) @?= Just False
            , testCase "description defaults to empty when absent" $
                let json = "{\"name\":\"alice\"}"
                 in fmap description (decode json :: Maybe UserConfig) @?= Just ""
            , testCase "rejects empty user name in JSON" $
                (decode "{\"name\":\"\"}" :: Maybe UserConfig) @?= Nothing
            , testGroup
                "properties"
                [ testProperty "JSON roundtrip" $
                    \(cfg :: UserConfig) -> decode (encode cfg) === Just cfg
                ]
            ]
        ]

requireRight :: (Show e) => Either e a -> a
requireRight (Right v) = v
requireRight (Left e) = error $ "requireRight: " <> show e
