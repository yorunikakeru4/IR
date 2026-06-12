{-# LANGUAGE OverloadedStrings #-}

module IR.TypesSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.List (isInfixOf)
import qualified IR.Domain.Service as Service
import IR.Types
import Test.Tasty
import Test.Tasty.HUnit

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
                    not ("baseState" `isInfixOf` BSC.unpack (encode (section Nothing)))
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
