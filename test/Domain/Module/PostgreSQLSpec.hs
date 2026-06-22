{-# LANGUAGE OverloadedStrings #-}

module Domain.Module.PostgreSQLSpec (tests) where

import Data.Aeson (decode, encode)
import Domain.Module (mkModuleName)
import Domain.Module.PostgreSQL
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Domain.Module.PostgreSQL"
        [ testCase "round-trips minimal config" $
            let name = requireRight (mkModuleName "main")
                cfg =
                    PostgreSQLConfig
                        { configName = name
                        , enable = True
                        , port = Nothing
                        , dataDir = Nothing
                        , maxConnections = Nothing
                        , policies = []
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "round-trips full config" $
            let name = requireRight (mkModuleName "main")
                cfg =
                    PostgreSQLConfig
                        { configName = name
                        , enable = True
                        , port = Just 5432
                        , dataDir = Just "/var/lib/postgresql"
                        , maxConnections = Just 200
                        , policies = []
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "rejects empty name in JSON" $
            (decode "{\"name\":\"\",\"enable\":true}" :: Maybe PostgreSQLConfig) @?= Nothing
        , testCase "round-trips with different name" $
            let name = requireRight (mkModuleName "replica")
                cfg =
                    PostgreSQLConfig
                        { configName = name
                        , enable = False
                        , port = Just 5433
                        , dataDir = Nothing
                        , maxConnections = Nothing
                        , policies = []
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "omits max_connections when absent" $
            let name = requireRight (mkModuleName "main")
                cfg = PostgreSQLConfig name True Nothing Nothing Nothing []
             in assertBool "unexpected max_connections key" $
                    not ("max_connections" `elem` (words (show (encode cfg))))
        ]

requireRight :: (Show e) => Either e a -> a
requireRight (Right v) = v
requireRight (Left e) = error $ "requireRight: " <> show e
