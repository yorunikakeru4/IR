{-# LANGUAGE OverloadedStrings #-}

-- | Shared observation strategy types used across all condition domains.
module IR.ObserveStrategy (
    IntervalMs,
    mkIntervalMs,
    intervalMilliseconds,
    ObserveStrategy (..),
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (DomainError (InvalidInterval))

-- | Polling interval in milliseconds.
newtype IntervalMs = IntervalMs Int
    deriving (Eq, Show)

-- | Smart constructor for 'IntervalMs' that validates the input.
mkIntervalMs :: Int -> Either DomainError IntervalMs
mkIntervalMs ms
    | ms <= 0 = Left (InvalidInterval ms)
    | otherwise = Right (IntervalMs ms)

intervalMilliseconds :: IntervalMs -> Int
intervalMilliseconds (IntervalMs ms) = ms

-- | How the engine observes a condition over time.
data ObserveStrategy
    = Poll IntervalMs
    deriving (Eq, Show)

instance ToJSON ObserveStrategy where
    toJSON (Poll (IntervalMs ms)) =
        object ["type" .= ("poll" :: Text), "interval_ms" .= ms]

instance FromJSON ObserveStrategy where
    parseJSON = withObject "ObserveStrategy" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "poll" -> do
                ms <- o .: "interval_ms"
                either (fail . show) (pure . Poll) (mkIntervalMs ms)
            _unknownType -> fail $ "unknown observe strategy: " <> T.unpack t
