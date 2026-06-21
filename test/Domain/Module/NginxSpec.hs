{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.NginxSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isInfixOf)
import Domain.Module (mkModuleName)
import Domain.Module.Nginx
import Domain.Module.Nginx.VirtualHost
import qualified Domain.Network as Network
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
                    NginxConfig
                        { nginxConfigName = name
                        , nginxEnable = True
                        , nginxVirtualHosts =
                            [ NginxVirtualHost
                                { vhostDomain = "example.com"
                                , vhostHttpPort = Just 80
                                , vhostHttpsPort = Just 443
                                , vhostProxyPass = Just "http://backend:3000"
                                , vhostPolicies =
                                    [ NetworkPolicy
                                        (requireRight (Network.fallbackPolicy (Network.PortResource (Network.Port 80)) [Network.PortResource (Network.Port 8080)]))
                                    ]
                                }
                            ]
                        , nginxPolicies =
                            [NetworkPolicy (requireRight (Network.allowPortsPolicy (Network.ports [80, 443])))]
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "omits virtual_hosts when empty" $
            let name = requireRight (mkModuleName "nginx")
                cfg =
                    NginxConfig
                        { nginxConfigName = name
                        , nginxEnable = True
                        , nginxVirtualHosts = []
                        , nginxPolicies = []
                        }
             in assertBool "unexpected virtual_hosts key" $
                    not ("virtual_hosts" `isInfixOf` BSLC.unpack (encode cfg))
        , testCase "defaults virtual host policies to empty when absent" $
            let json = "{\"domain\":\"example.com\"}"
                expected =
                    NginxVirtualHost
                        { vhostDomain = "example.com"
                        , vhostHttpPort = Nothing
                        , vhostHttpsPort = Nothing
                        , vhostProxyPass = Nothing
                        , vhostPolicies = []
                        }
             in decode json @?= Just expected
        ]

requireRight :: (Show e) => Either e a -> a
requireRight (Right v) = v
requireRight (Left e) = error $ "requireRight: " <> show e
