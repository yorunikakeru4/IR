{-# LANGUAGE OverloadedStrings #-}

module Domain.User (
    UserName,
    mkUserName,
    userNameText,
    UserConfig (..),
    emptyUserConfig,
) where

import Data.Aeson
import Data.Text (Text)
import Domain.Error (DomainError, mkName, parseDomain)
import Domain.Package (PackageName)

newtype UserName = UserName Text
    deriving (Eq, Show)

mkUserName :: Text -> Either DomainError UserName
mkUserName = mkName UserName

userNameText :: UserName -> Text
userNameText (UserName n) = n

instance ToJSON UserName where
    toJSON (UserName n) = toJSON n

instance FromJSON UserName where
    parseJSON value = do
        n <- parseJSON value
        parseDomain (mkUserName n)

data UserConfig = UserConfig
    { userName :: UserName
    , isNormalUser :: Bool
    , description :: Text
    , extraGroups :: [Text]
    , packages :: [PackageName]
    }
    deriving (Eq, Show)

emptyUserConfig :: UserConfig
emptyUserConfig =
    UserConfig
        { userName = UserName "user"
        , isNormalUser = False
        , description = ""
        , extraGroups = []
        , packages = []
        }

instance ToJSON UserConfig where
    toJSON uc =
        object $
            ["name" .= userName uc, "normal_user" .= isNormalUser uc]
                <> (if description uc == "" then [] else ["description" .= description uc])
                <> (if null (extraGroups uc) then [] else ["extra_groups" .= extraGroups uc])
                <> (if null (packages uc) then [] else ["packages" .= packages uc])

instance FromJSON UserConfig where
    parseJSON = withObject "UserConfig" $ \o -> do
        rawName <- o .: "name"
        name <- parseDomain (mkUserName rawName)
        UserConfig name
            <$> o .:? "normal_user" .!= False
            <*> o .:? "description" .!= ""
            <*> o .:? "extra_groups" .!= []
            <*> o .:? "packages" .!= []
