{-# LANGUAGE OverloadedStrings #-}

module IR.Domain.Module.Docker (
    StorageDriver (..),
    DockerConfig (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

data StorageDriver = Overlay2 | VFS | BTRFS
    deriving (Eq, Show)

instance ToJSON StorageDriver where
    toJSON Overlay2 = "overlay2"
    toJSON VFS = "vfs"
    toJSON BTRFS = "btrfs"

instance FromJSON StorageDriver where
    parseJSON = withText "StorageDriver" $ \t -> case t of
        "overlay2" -> pure Overlay2
        "vfs" -> pure VFS
        "btrfs" -> pure BTRFS
        _unknown -> fail $ "unknown storage driver: " <> T.unpack t

-- | Docker is a singleton module; name is always serialised as "default".
data DockerConfig = DockerConfig
    { dockerEnable :: Bool
    , dockerRootless :: Bool
    , dockerStorageDriver :: Maybe StorageDriver
    }
    deriving (Eq, Show)

instance ToJSON DockerConfig where
    toJSON dc =
        object $
            [ "name" .= ("default" :: Text)
            , "enable" .= dockerEnable dc
            , "rootless" .= dockerRootless dc
            ]
                <> maybe [] (\sd -> ["storage_driver" .= sd]) (dockerStorageDriver dc)

instance FromJSON DockerConfig where
    parseJSON = withObject "DockerConfig" $ \o ->
        DockerConfig
            <$> o .: "enable"
            <*> o .:? "rootless" .!= False
            <*> o .:? "storage_driver"
