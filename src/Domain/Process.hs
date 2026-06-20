{-# LANGUAGE OverloadedStrings #-}

-- | Process-related IR nodes: conditions for observing running processes.
module Domain.Process (
    ProcessName,
    mkProcessName,
    processNameText,
    AppName,
    mkAppName,
    appNameText,
    IntervalMs,
    mkIntervalMs,
    intervalMilliseconds,
    ObserveStrategy (..),
    Condition (..),
    attachStrategy,
)
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Error (DomainError (StrategyConflict), mkName, renderDomainError)
import ObserveStrategy (IntervalMs, ObserveStrategy (..), intervalMilliseconds, mkIntervalMs)

-- | Name of a system process as it appears in the process table.
newtype ProcessName = ProcessName Text
    deriving (Eq, Show)

mkProcessName :: Text -> Either DomainError ProcessName
mkProcessName = mkName ProcessName

processNameText :: ProcessName -> Text
processNameText (ProcessName name) = name

-- | FrogOS app/package identity as declared in package metadata.
newtype AppName = AppName Text
    deriving (Eq, Show)

mkAppName :: Text -> Either DomainError AppName
mkAppName = mkName AppName

appNameText :: AppName -> Text
appNameText (AppName name) = name

-- | Process-domain conditions.
data Condition
    = -- | True while the named process is present in the process table.
      ProcessRunning ProcessName (Maybe ObserveStrategy)
    | -- | True while any process belonging to the named FrogOS app is running.
      AppRunning AppName (Maybe ObserveStrategy)
    deriving (Eq, Show)

attachStrategy :: ObserveStrategy -> Condition -> Either DomainError Condition
attachStrategy strategy (ProcessRunning name Nothing) =
    Right (ProcessRunning name (Just strategy))
attachStrategy _strategy (ProcessRunning _name (Just _existingStrategy)) =
    Left StrategyConflict
attachStrategy strategy (AppRunning name Nothing) =
    Right (AppRunning name (Just strategy))
attachStrategy _strategy (AppRunning _name (Just _existingStrategy)) =
    Left StrategyConflict

instance ToJSON Condition where
    toJSON (ProcessRunning (ProcessName n) obs) =
        object $
            ["type" .= ("process_running" :: Text), "name" .= n]
                <> maybe [] (\s -> ["observe" .= s]) obs
    toJSON (AppRunning (AppName n) obs) =
        object $
            ["type" .= ("app_running" :: Text), "name" .= n]
                <> maybe [] (\s -> ["observe" .= s]) obs

instance FromJSON Condition where
    parseJSON = withObject "Process.Condition" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "process_running" -> do
                name <- o .: "name"
                processName <- either (fail . T.unpack . renderDomainError) pure (mkProcessName name)
                ProcessRunning processName <$> o .:? "observe"
            "app_running" -> do
                name <- o .: "name"
                appName <- either (fail . T.unpack . renderDomainError) pure (mkAppName name)
                AppRunning appName <$> o .:? "observe"
            _unknownType -> fail $ "unknown process condition: " <> T.unpack t
