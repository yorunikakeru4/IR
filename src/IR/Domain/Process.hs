{-# LANGUAGE OverloadedStrings #-}

-- | Process-related IR nodes: conditions for observing running processes.
module IR.Domain.Process (
    ProcessName,
    mkProcessName,
    processNameText,
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
import IR.Domain.Error (DomainError (StrategyConflict), mkName, renderDomainError)
import IR.ObserveStrategy (IntervalMs, ObserveStrategy (..), intervalMilliseconds, mkIntervalMs)

-- | Name of a system process as it appears in the process table.
newtype ProcessName = ProcessName Text
    deriving (Eq, Show)

mkProcessName :: Text -> Either DomainError ProcessName
mkProcessName = mkName ProcessName

processNameText :: ProcessName -> Text
processNameText (ProcessName name) = name

-- | Process-domain conditions.
data Condition
    = -- | True while the named process is present in the process table.
      ProcessRunning ProcessName (Maybe ObserveStrategy)
    deriving (Eq, Show)

attachStrategy :: ObserveStrategy -> Condition -> Either DomainError Condition
attachStrategy strategy (ProcessRunning name Nothing) =
    Right (ProcessRunning name (Just strategy))
attachStrategy _strategy (ProcessRunning _name (Just _existingStrategy)) =
    Left StrategyConflict

instance ToJSON Condition where
    toJSON (ProcessRunning (ProcessName n) obs) =
        object $
            ["type" .= ("process_running" :: Text), "name" .= n]
                <> maybe [] (\s -> ["observe" .= s]) obs

instance FromJSON Condition where
    parseJSON = withObject "Process.Condition" $ \o -> do
        t <- o .: "type" :: Parser Text
        case t of
            "process_running" -> do
                name <- o .: "name"
                processName <- either (fail . T.unpack . renderDomainError) pure (mkProcessName name)
                ProcessRunning processName <$> o .:? "observe"
            _unknownType -> fail $ "unknown process condition: " <> T.unpack t
