module Main (main) where

import qualified IR.Domain.ErrorSpec
import qualified IR.Domain.NetworkSpec
import qualified IR.Domain.PackageSpec
import qualified IR.Domain.PowerSpec
import qualified IR.Domain.ProcessSpec
import qualified IR.Domain.ServiceSpec
import qualified IR.Domain.SystemSpec
import qualified IR.TypesSpec
import Test.Tasty

main :: IO ()
main =
    defaultMain $
        testGroup
            "IR"
            [ IR.Domain.ErrorSpec.tests
            , IR.Domain.NetworkSpec.tests
            , IR.Domain.PackageSpec.tests
            , IR.Domain.PowerSpec.tests
            , IR.Domain.ProcessSpec.tests
            , IR.Domain.ServiceSpec.tests
            , IR.Domain.SystemSpec.tests
            , IR.TypesSpec.tests
            ]
