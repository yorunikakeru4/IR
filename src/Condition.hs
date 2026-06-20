{-# LANGUAGE OverloadedStrings #-}

-- | Closed root sum of all observable conditions across domains.
module Condition (
    Condition (..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Domain.Process as Process
import qualified Domain.System as System

{- | A boolean predicate over observable system state.
Adding a new condition domain requires extending this sum and bumping 'Types.IRVersion'.
-}
data Condition
    = ProcessCondition Process.Condition
    | SystemCondition System.Condition
    | And Condition Condition
    | Not Condition
    deriving (Eq, Show)

instance ToJSON Condition where
    toJSON (ProcessCondition c) = toJSON c
    toJSON (SystemCondition c) = toJSON c
    toJSON (And l r) = object ["type" .= ("and" :: Text), "left" .= l, "right" .= r]
    toJSON (Not c) = object ["type" .= ("not" :: Text), "condition" .= c]

instance FromJSON Condition where
    parseJSON v =
        withObject
            "Condition"
            ( \o -> do
                t <- o .: "type" :: Parser Text
                case t of
                    "process_running" -> ProcessCondition <$> parseJSON v
                    "app_running" -> ProcessCondition <$> parseJSON v
                    "cpu_load" -> SystemCondition <$> parseJSON v
                    "battery_below" -> SystemCondition <$> parseJSON v
                    "battery_above" -> SystemCondition <$> parseJSON v
                    "and" -> And <$> o .: "left" <*> o .: "right"
                    "not" -> Not <$> o .: "condition"
                    _unknownType -> fail $ "unknown condition type: " <> T.unpack t
            )
            v
