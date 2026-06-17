{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Power (
    PowerProfile (..),
    parsePowerProfile,
    renderPowerProfile,
    Action (..),
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import IR.Domain.Error (DomainError (..))

data PowerProfile
    = Performance
    | Balanced
    | PowerSave
    deriving (Eq, Show, Generic)

parsePowerProfile :: Text -> Either DomainError PowerProfile
parsePowerProfile "performance" = Right Performance
parsePowerProfile "balanced" = Right Balanced
parsePowerProfile "powersave" = Right PowerSave
parsePowerProfile t = Left (UnknownPowerProfile t)

renderPowerProfile :: PowerProfile -> Text
renderPowerProfile Performance = "performance"
renderPowerProfile Balanced = "balanced"
renderPowerProfile PowerSave = "powersave"

data Action
    = SetPowerProfile PowerProfile
    deriving (Eq, Show)

powerProfileOptions :: Options
powerProfileOptions = defaultOptions{constructorTagModifier = map toLower}

instance ToJSON PowerProfile where
    toJSON = genericToJSON powerProfileOptions

instance FromJSON PowerProfile where
    parseJSON = genericParseJSON powerProfileOptions

instance ToJSON Action where
    toJSON (SetPowerProfile p) =
        object ["type" .= ("power_profile" :: Text), "value" .= p]

instance FromJSON Action where
    parseJSON = withObject "Power.Action" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "power_profile" -> SetPowerProfile <$> o .: "value"
            _unknownType -> fail $ "unknown power action: " <> T.unpack t
