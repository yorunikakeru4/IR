{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.NginxSpec (tests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isInfixOf)
import Domain.Module (mkModuleName)
import Domain.Module.Nginx
import Domain.Module.Nginx.VirtualHost
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
                        , nginxHttpPort = Nothing
                        , nginxHttpsPort = Nothing
                        , nginxDomain = Nothing
                        , nginxVirtualHosts =
                            [ NginxVirtualHost
                                { vhostDomain = "example.com"
                                , vhostHttpPort = Just 80
                                , vhostHttpsPort = Just 443
                                , vhostProxyPass = Just "http://backend:3000"
                                }
                            ]
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "omits virtual_hosts when empty" $
            let name = requireRight (mkModuleName "nginx")
                cfg =
                    NginxConfig
                        { nginxConfigName = name
                        , nginxEnable = True
                        , nginxHttpPort = Nothing
                        , nginxHttpsPort = Nothing
                        , nginxDomain = Nothing
                        , nginxVirtualHosts = []
                        }
             in assertBool "unexpected virtual_hosts key" $
                    not ("virtual_hosts" `isInfixOf` BSLC.unpack (encode cfg))
        ]

requireRight :: (Show e) => Either e a -> a
requireRight (Right v) = v
requireRight (Left e) = error $ "requireRight: " <> show e
