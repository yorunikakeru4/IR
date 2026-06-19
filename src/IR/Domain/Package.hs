{-# LANGUAGE OverloadedStrings #-}

-- | Package-related IR nodes: declarations of FrogOS-managed packages.
module IR.Domain.Package (
    PackageName,
    mkPackageName,
    packageNameText,
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import IR.Domain.Error (DomainError, mkName, renderDomainError)

-- | Name of a FrogOS-managed package as declared in package metadata.
newtype PackageName = PackageName Text
    deriving (Eq, Show)

mkPackageName :: Text -> Either DomainError PackageName
mkPackageName = mkName PackageName

packageNameText :: PackageName -> Text
packageNameText (PackageName name) = name

instance ToJSON PackageName where
    toJSON (PackageName n) = toJSON n

instance FromJSON PackageName where
    parseJSON value = do
        name <- parseJSON value
        either (fail . T.unpack . renderDomainError) pure (mkPackageName name)
