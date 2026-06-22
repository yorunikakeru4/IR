{-# LANGUAGE OverloadedStrings #-}

-- | Shared observation strategy types used across all condition domains.
module ObserveStrategy (
    IntervalMs,
    mkIntervalMs,
    intervalMilliseconds,
    ObserveStrategy (..),
    observeField,
) where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (DomainError (InvalidInterval), parseDomain)

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

observeField :: Maybe ObserveStrategy -> [Pair]
observeField = maybe [] (\s -> ["observe" .= s])

instance ToJSON ObserveStrategy where
    toJSON (Poll (IntervalMs ms)) =
        object ["type" .= ("poll" :: Text), "interval_ms" .= ms]

instance FromJSON ObserveStrategy where
    parseJSON = withObject "ObserveStrategy" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "poll" -> do
                ms <- o .: "interval_ms"
                Poll <$> parseDomain (mkIntervalMs ms)
            _unknownType -> fail $ "unknown observe strategy: " <> T.unpack t
