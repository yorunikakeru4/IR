{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.PackageSpec (tests) where

import Arbitraries ()
import Data.Aeson (decode, encode)
import Domain.Error (DomainError (EmptyName))
import Domain.Package
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "Domain.Package"
        [ testCase "rejects empty package name" $
            mkPackageName "" @?= Left EmptyName
        , testCase "accepts non-empty package name" $
            fmap packageNameText (mkPackageName "steam") @?= Right "steam"
        , testCase "empty package name JSON is rejected" $
            (decode "\"\"" :: Maybe PackageName) @?= Nothing
        , testGroup
            "properties"
            [ testProperty "packageNameText . mkPackageName roundtrips non-empty text" $
                \name ->
                    fmap packageNameText (mkPackageName (packageNameText name))
                        === Right (packageNameText name)
            , testProperty "JSON roundtrip for PackageName" $
                \(name :: PackageName) -> decode (encode name) === Just name
            ]
        ]
