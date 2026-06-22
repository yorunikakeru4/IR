{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.NginxSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isInfixOf)
import Domain.Module (mkModuleName)
import qualified Domain.Module.Nginx as NG
import qualified Domain.Module.Nginx.VirtualHost as VH
import qualified Domain.Network as Network (Resource (..), Port (..), fallbackPolicy)
import Policy (Policy (NetworkPolicy))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Domain.Module.Nginx"
        [ testCase "round-trips config with virtual hosts" $
            let name = requireRight (mkModuleName "nginx")
                cfg =
                    NG.NginxConfig
                        { NG.configName = name
                        , NG.enable = True
                        , NG.virtualHosts =
                            [ VH.NginxVirtualHost
                                { VH.domain = "example.com"
                                , VH.httpPort = Just 80
                                , VH.httpsPort = Just 443
                                , VH.proxyPass = Just "http://backend:3000"
                                , VH.policies =
                                    [ NetworkPolicy
                                        (requireRight (Network.fallbackPolicy (Network.PortResource (Network.Port 80)) [Network.PortResource (Network.Port 8080)]))
                                    ]
                                }
                            ]
                        , NG.policies = []
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "omits virtual_hosts when empty" $
            let name = requireRight (mkModuleName "nginx")
                cfg =
                    NG.NginxConfig
                        { NG.configName = name
                        , NG.enable = True
                        , NG.virtualHosts = []
                        , NG.policies = []
                        }
             in assertBool "unexpected virtual_hosts key" $
                    not ("virtual_hosts" `isInfixOf` BSLC.unpack (encode cfg))
        , testCase "defaults virtual host policies to empty when absent" $
            let json = "{\"domain\":\"example.com\"}"
                expected =
                    VH.NginxVirtualHost
                        { VH.domain = "example.com"
                        , VH.httpPort = Nothing
                        , VH.httpsPort = Nothing
                        , VH.proxyPass = Nothing
                        , VH.policies = []
                        }
             in decode json @?= Just expected
        ]

requireRight :: (Show e) => Either e a -> a
requireRight (Right v) = v
requireRight (Left e) = error $ "requireRight: " <> show e
