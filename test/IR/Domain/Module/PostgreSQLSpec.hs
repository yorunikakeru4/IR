{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.PostgreSQLSpec (tests) where

import Data.Aeson (decode, encode)
import IR.Domain.Error (DomainError (..))
import IR.Domain.Module (mkModuleName)
import IR.Domain.Module.PostgreSQL
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "IR.Domain.Module.PostgreSQL"
        [ testCase "round-trips minimal config" $
            let Right name = mkModuleName "main"
                cfg =
                    PostgreSQLConfig
                        { postgresqlConfigName = name
                        , postgresqlEnable = True
                        , postgresqlPort = Nothing
                        , postgresqlDataDir = Nothing
                        , postgresqlMaxConnections = Nothing
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "round-trips full config" $
            let Right name = mkModuleName "main"
                cfg =
                    PostgreSQLConfig
                        { postgresqlConfigName = name
                        , postgresqlEnable = True
                        , postgresqlPort = Just 5432
                        , postgresqlDataDir = Just "/var/lib/postgresql"
                        , postgresqlMaxConnections = Just 200
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "rejects empty name in JSON" $
            (decode "{\"name\":\"\",\"enable\":true}" :: Maybe PostgreSQLConfig) @?= Nothing
        , testCase "round-trips with different name" $
            let Right name = mkModuleName "replica"
                cfg =
                    PostgreSQLConfig
                        { postgresqlConfigName = name
                        , postgresqlEnable = False
                        , postgresqlPort = Just 5433
                        , postgresqlDataDir = Nothing
                        , postgresqlMaxConnections = Nothing
                        }
             in decode (encode cfg) @?= Just cfg
        , testCase "omits max_connections when absent" $
            let Right name = mkModuleName "main"
                cfg = PostgreSQLConfig name True Nothing Nothing Nothing
             in assertBool "unexpected max_connections key" $
                    not ("max_connections" `elem` (words (show (encode cfg))))
        ]
