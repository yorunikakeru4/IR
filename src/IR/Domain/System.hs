{-# LANGUAGE OverloadedStrings #-}

-- | System-metric IR nodes: conditions over CPU load and battery state.
module IR.Domain.System (
    CpuLoadThreshold,
    mkCpuLoadThreshold,
    cpuLoadThresholdValue,
    BatteryPercent,
    mkBatteryPercent,
    batteryPercentValue,
    Condition (..),
    attachStrategy,
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (DomainError (StrategyConflict), mkPercent, mkThreshold)
import IR.ObserveStrategy (ObserveStrategy)

newtype CpuLoadThreshold = CpuLoadThreshold Double
    deriving (Eq, Show)

mkCpuLoadThreshold :: Double -> Either DomainError CpuLoadThreshold
mkCpuLoadThreshold = mkThreshold CpuLoadThreshold

cpuLoadThresholdValue :: CpuLoadThreshold -> Double
cpuLoadThresholdValue (CpuLoadThreshold threshold) = threshold

newtype BatteryPercent = BatteryPercent Int
    deriving (Eq, Show)

mkBatteryPercent :: Int -> Either DomainError BatteryPercent
mkBatteryPercent = mkPercent BatteryPercent

batteryPercentValue :: BatteryPercent -> Int
batteryPercentValue (BatteryPercent percent) = percent

-- | System-domain conditions.
data Condition
    = -- | True while CPU load fraction is at or above threshold (0.0-1.0).
      CpuLoad CpuLoadThreshold (Maybe ObserveStrategy)
    | -- | True while battery charge is below the given percentage (0-100).
      BatteryBelow BatteryPercent (Maybe ObserveStrategy)
    | -- | True while battery charge is above the given percentage (0-100).
      BatteryAbove BatteryPercent (Maybe ObserveStrategy)
    deriving (Eq, Show)

attachStrategy :: ObserveStrategy -> Condition -> Either DomainError Condition
attachStrategy strategy (CpuLoad threshold Nothing) =
    Right (CpuLoad threshold (Just strategy))
attachStrategy _strategy (CpuLoad _threshold (Just _existingStrategy)) =
    Left StrategyConflict
attachStrategy strategy (BatteryBelow percent Nothing) =
    Right (BatteryBelow percent (Just strategy))
attachStrategy _strategy (BatteryBelow _percent (Just _existingStrategy)) =
    Left StrategyConflict
attachStrategy strategy (BatteryAbove percent Nothing) =
    Right (BatteryAbove percent (Just strategy))
attachStrategy _strategy (BatteryAbove _percent (Just _existingStrategy)) =
    Left StrategyConflict

instance ToJSON Condition where
    toJSON (CpuLoad t obs) =
        object $
            ["type" .= ("cpu_load" :: Text), "threshold" .= cpuLoadThresholdValue t]
                <> maybe [] (\s -> ["observe" .= s]) obs
    toJSON (BatteryBelow p obs) =
        object $
            ["type" .= ("battery_below" :: Text), "percent" .= batteryPercentValue p]
                <> maybe [] (\s -> ["observe" .= s]) obs
    toJSON (BatteryAbove p obs) =
        object $
            ["type" .= ("battery_above" :: Text), "percent" .= batteryPercentValue p]
                <> maybe [] (\s -> ["observe" .= s]) obs

instance FromJSON Condition where
    parseJSON = withObject "System.Condition" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "cpu_load" -> do
                raw <- o .: "threshold"
                threshold <- either (fail . show) pure (mkCpuLoadThreshold raw)
                CpuLoad threshold <$> o .:? "observe"
            "battery_below" -> do
                raw <- o .: "percent"
                percent <- either (fail . show) pure (mkBatteryPercent raw)
                BatteryBelow percent <$> o .:? "observe"
            "battery_above" -> do
                raw <- o .: "percent"
                percent <- either (fail . show) pure (mkBatteryPercent raw)
                BatteryAbove percent <$> o .:? "observe"
            _unknownType -> fail $ "unknown system condition: " <> T.unpack t
