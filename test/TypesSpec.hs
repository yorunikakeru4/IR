{-# LANGUAGE OverloadedStrings #-}

module TypesSpec (tests) where

import Arbitraries ()
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Domain.Service as Service
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Types

tests :: TestTree
tests =
    testGroup
        "Types"
        [ testGroup
            "ServiceSection baseState JSON"
            [ testCase "round-trips a declared base state" $
                decode (encode (section (Just Service.Enabled)))
                    @?= Just (section (Just Service.Enabled))
            , testCase "decodes legacy JSON without baseState to Nothing" $
                decode "{\"name\":\"backend\",\"policies\":[],\"actions\":[]}"
                    @?= Just (section Nothing)
            , testCase "omits baseState key when absent" $
                assertBool "unexpected baseState key" $
                    not ("\"baseState\"" `BS.isInfixOf` BSL.toStrict (encode (section Nothing)))
            ]
        , testGroup
            "packages JSON"
            [ testCase "decodes legacy JSON without packages to empty list" $
                decode "{\"name\":\"backend\",\"policies\":[],\"actions\":[]}"
                    @?= Just (section Nothing)
            , testCase "omits packages key when list is empty" $
                assertBool "unexpected packages key" $
                    not ("\"packages\"" `BS.isInfixOf` BSL.toStrict (encode (section Nothing)))
            ]
        , testGroup
            "modules JSON"
            [ testCase "omits modules key when ModuleMap is empty" $
                assertBool "unexpected modules key" $
                    not ("\"modules\"" `BS.isInfixOf` BSL.toStrict (encode emptyDoc))
            , testCase "decodes JSON without modules to emptyModuleMap" $
                fmap irModules (decode "{\"version\":3,\"profiles\":[],\"services\":[]}" :: Maybe IRDocument)
                    @?= Just emptyModuleMap
            , testCase "round-trips emptyModuleMap" $
                decode (encode emptyDoc) @?= Just emptyDoc
            ]
        , testCase "currentIRVersion is 4" $
            currentIRVersion @?= IRVersion 4
        , testGroup
            "properties"
            [ testProperty "profileNameText . mkProfileName roundtrips non-empty text" $
                \name ->
                    fmap profileNameText (mkProfileName (profileNameText name))
                        === Right (profileNameText name)
            , testProperty "serviceSectionNameText . mkServiceSectionName roundtrips non-empty text" $
                \name ->
                    fmap serviceSectionNameText (mkServiceSectionName (serviceSectionNameText name))
                        === Right (serviceSectionNameText name)
            ]
        ]

emptyDoc :: IRDocument
emptyDoc =
    IRDocument
        { irVersion = currentIRVersion
        , irPackages = []
        , irModules = emptyModuleMap
        , irProfiles = []
        , irServices = []
        }

section :: Maybe Service.ServiceBaseState -> ServiceSection
section base =
    ServiceSection
        { serviceSectionName = name
        , serviceSectionBaseState = base
        , serviceSectionPolicies = []
        , serviceSectionActions = []
        , serviceSectionPackages = []
        }
  where
    name = either (error . show) id (mkServiceSectionName "backend")
