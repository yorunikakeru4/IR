{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypesSpec (tests) where

import Arbitraries ()
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Domain.Package as Package
import qualified Domain.User as User
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
                    not ("\"packages\"" `BS.isInfixOf` BSL.toStrict (encode emptyIRDocument))
            ]
        , testGroup
            "modules JSON"
            [ testCase "omits modules key when ModuleMap is empty" $
                assertBool "unexpected modules key" $
                    not ("\"modules\"" `BS.isInfixOf` BSL.toStrict (encode emptyIRDocument))
            , testCase "decodes JSON without modules to modulesEmpty" $
                fmap irModules (decode "{\"version\":5,\"profiles\":[]}" :: Maybe IRDocument)
                    @?= Just emptyModules
            , testCase "round-trips emptyModuleMap" $
                decode (encode emptyIRDocument) @?= Just emptyIRDocument
            ]
        , testCase "omits services key" $
            assertBool "unexpected services key" $
                not ("\"services\"" `BS.isInfixOf` BSL.toStrict (encode emptyIRDocument))
        , testCase "currentIRVersion is 9" $
            currentIRVersion @?= IRVersion 9
        , testGroup
            "properties"
            [ testProperty "profileNameText . mkProfileName roundtrips non-empty text" $
                \name ->
                    fmap profileNameText (mkProfileName (profileNameText name))
                        === Right (profileNameText name)
            , testProperty "UserConfig shrink preserves non-empty groups" $
                \(cfg :: User.UserConfig) ->
                    all (all (not . T.null) . User.extraGroups) (shrink cfg)
                        === True
            , testProperty "IRDocument users with packages and groups JSON roundtrip" $
                \(cfg :: User.UserConfig) (pkg :: Package.PackageName) (NonEmpty groupChars) ->
                    let user =
                            cfg
                                { User.extraGroups = [T.pack groupChars]
                                , User.packages = [pkg]
                                }
                        doc = emptyIRDocument{irUsers = [user]}
                     in decode (encode doc) === Just doc
            , testProperty "IRDocument packages and network JSON roundtrip" $
                \(doc :: IRDocument) -> decode (encode doc) === Just doc
            ]
        ]
