{-# LANGUAGE OverloadedStrings #-}

module IR.TypesSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import IR.Arbitraries ()
import qualified IR.Domain.Service as Service
import IR.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "IR.Types"
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

section :: Maybe Service.ServiceBaseState -> ServiceSection
section base =
    ServiceSection
        { serviceSectionName = name
        , serviceSectionBaseState = base
        , serviceSectionPolicies = []
        , serviceSectionActions = []
        }
  where
    name = either (error . show) id (mkServiceSectionName "backend")
