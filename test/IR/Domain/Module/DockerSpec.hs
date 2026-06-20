{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.DockerSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isInfixOf)
import IR.Domain.Module.Docker
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "IR.Domain.Module.Docker"
        [ testGroup
            "StorageDriver JSON"
            [ testCase "encodes Overlay2 as \"overlay2\"" $
                encode Overlay2 @?= "\"overlay2\""
            , testCase "encodes VFS as \"vfs\"" $
                encode VFS @?= "\"vfs\""
            , testCase "encodes BTRFS as \"btrfs\"" $
                encode BTRFS @?= "\"btrfs\""
            , testCase "decodes \"overlay2\" to Overlay2" $
                decode "\"overlay2\"" @?= Just Overlay2
            , testCase "decodes \"vfs\" to VFS" $
                decode "\"vfs\"" @?= Just VFS
            , testCase "rejects unknown driver" $
                (decode "\"zfs\"" :: Maybe StorageDriver) @?= Nothing
            ]
        , testGroup
            "DockerConfig JSON"
            [ testCase "round-trips minimal config" $
                let cfg = DockerConfig{dockerEnable = True, dockerRootless = False, dockerStorageDriver = Nothing}
                 in decode (encode cfg) @?= Just cfg
            , testCase "round-trips full config" $
                let cfg = DockerConfig{dockerEnable = True, dockerRootless = True, dockerStorageDriver = Just Overlay2}
                 in decode (encode cfg) @?= Just cfg
            , testCase "serialised JSON contains name=default" $
                let cfg = DockerConfig{dockerEnable = True, dockerRootless = False, dockerStorageDriver = Nothing}
                 in assertBool "missing default" $
                        "\"default\"" `isInfixOf` BSLC.unpack (encode cfg)
            , testCase "omits storage_driver when absent" $
                let cfg = DockerConfig{dockerEnable = True, dockerRootless = False, dockerStorageDriver = Nothing}
                 in assertBool "unexpected storage_driver" $
                        not ("storage_driver" `isInfixOf` BSLC.unpack (encode cfg))
            , testCase "includes storage_driver when present" $
                let cfg = DockerConfig{dockerEnable = True, dockerRootless = True, dockerStorageDriver = Just Overlay2}
                 in assertBool "missing storage_driver" $
                        "storage_driver" `isInfixOf` BSLC.unpack (encode cfg)
            ]
        ]
