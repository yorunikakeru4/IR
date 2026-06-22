module Main (main) where

import qualified Domain.ErrorSpec
import qualified Domain.Module.NginxSpec
import qualified Domain.Module.PostgreSQLSpec
import qualified Domain.ModuleSpec
import qualified Domain.NetworkSpec
import qualified Domain.PackageSpec
import qualified Domain.PowerSpec
import qualified Domain.ProcessSpec
import qualified Domain.SystemSpec
import qualified Domain.UserSpec
import Test.Tasty
import qualified TypesSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "IR"
            [ Domain.ErrorSpec.tests
            , Domain.Module.NginxSpec.tests
            , Domain.Module.PostgreSQLSpec.tests
            , Domain.ModuleSpec.tests
            , Domain.NetworkSpec.tests
            , Domain.PackageSpec.tests
            , Domain.PowerSpec.tests
            , Domain.ProcessSpec.tests
            , Domain.SystemSpec.tests
            , Domain.UserSpec.tests
            , TypesSpec.tests
            ]
