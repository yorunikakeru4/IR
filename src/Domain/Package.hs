{-# LANGUAGE OverloadedStrings #-}

-- | Package-related IR nodes: declarations of FrogOS-managed packages.
module Domain.Package (
    PackageName,
    mkPackageName,
    packageNameText,
) where

import Data.Aeson
import Data.Text (Text)
import Domain.Error (DomainError, mkName, parseDomain)

-- | Name of a FrogOS-managed package as declared in package metadata.
newtype PackageName = PackageName Text
    deriving (Eq, Ord, Show)

mkPackageName :: Text -> Either DomainError PackageName
mkPackageName = mkName PackageName

packageNameText :: PackageName -> Text
packageNameText (PackageName name) = name

instance ToJSON PackageName where
    toJSON (PackageName n) = toJSON n

instance FromJSON PackageName where
    parseJSON value = do
        name <- parseJSON value
        parseDomain (mkPackageName name)
