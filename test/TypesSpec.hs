{-# LANGUAGE OverloadedStrings #-}

module TypesSpec (tests) where

import Arbitraries ()
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Types

tests :: TestTree
tests =
    testGroup
        "Types"
        [ testGroup
            "packages JSON"
            [ testCase "omits packages key when list is empty" $
                assertBool "unexpected packages key" $
                    not ("\"packages\"" `BS.isInfixOf` BSL.toStrict (encode emptyDoc))
            ]
        , testGroup
            "modules JSON"
            [ testCase "omits modules key when ModuleMap is empty" $
                assertBool "unexpected modules key" $
                    not ("\"modules\"" `BS.isInfixOf` BSL.toStrict (encode emptyDoc))
            , testCase "decodes JSON without modules to emptyModuleMap" $
                fmap irModules (decode "{\"version\":5,\"profiles\":[]}" :: Maybe IRDocument)
                    @?= Just emptyModuleMap
            , testCase "round-trips emptyModuleMap" $
                decode (encode emptyDoc) @?= Just emptyDoc
            ]
        , testCase "omits services key" $
            assertBool "unexpected services key" $
                not ("\"services\"" `BS.isInfixOf` BSL.toStrict (encode emptyDoc))
        , testCase "currentIRVersion is 5" $
            currentIRVersion @?= IRVersion 5
        , testGroup
            "properties"
            [ testProperty "profileNameText . mkProfileName roundtrips non-empty text" $
                \name ->
                    fmap profileNameText (mkProfileName (profileNameText name))
                        === Right (profileNameText name)
            ]
        ]

emptyDoc :: IRDocument
emptyDoc =
    IRDocument
        { irVersion = currentIRVersion
        , irPackages = []
        , irModules = emptyModuleMap
        , irProfiles = []
        }
