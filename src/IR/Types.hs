{-# LANGUAGE DeriveGeneric #-}

{- | Core IR document structure: the top-level intent representation
consumed by the FrogOS Planner.
-}
module IR.Types (
    IRVersion (..),
    currentIRVersion,
    ProfileName,
    mkProfileName,
    profileNameText,
    ServiceSectionName,
    mkServiceSectionName,
    serviceSectionNameText,
    ProfileSection (..),
    ServiceSection (..),
    IRDocument (..),
) where

import Data.Aeson
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Text (Text)
import GHC.Generics (Generic)
import IR.Action (Action)
import IR.Condition (Condition)
import IR.Domain.Error (DomainError, mkName)
import qualified IR.Domain.Service as Service
import IR.Policy (Policy)

{- | Monotonically incrementing IR schema version.
A version mismatch causes the engine to reject the document and roll back.
-}
newtype IRVersion = IRVersion Int
    deriving (Eq, Show)

{- | The current IR schema version. Bump this whenever the 'IR.Condition.Condition'
or 'IR.Action.Action' sums are extended with new constructors.
-}
currentIRVersion :: IRVersion
currentIRVersion = IRVersion 2

-- | User-defined name for a profile section.
newtype ProfileName = ProfileName Text
    deriving (Eq, Show)

-- | User-defined name for a service section.
newtype ServiceSectionName = ServiceSectionName Text
    deriving (Eq, Show)

mkProfileName :: Text -> Either DomainError ProfileName
mkProfileName = mkName ProfileName

profileNameText :: ProfileName -> Text
profileNameText (ProfileName name) = name

mkServiceSectionName :: Text -> Either DomainError ServiceSectionName
mkServiceSectionName = mkName ServiceSectionName

serviceSectionNameText :: ServiceSectionName -> Text
serviceSectionNameText (ServiceSectionName name) = name

-- | A named group of conditions, policies, and actions applied when the profile is active.
data ProfileSection = ProfileSection
    { profileName :: ProfileName
    , profileCondition :: Maybe Condition
    , profilePolicies :: [Policy]
    , profileActions :: [Action]
    }
    deriving (Eq, Show, Generic)

-- | A named group of policies and actions for a service.
data ServiceSection = ServiceSection
    { serviceSectionName :: ServiceSectionName
    , serviceSectionBaseState :: Maybe Service.ServiceBaseState
    , serviceSectionPolicies :: [Policy]
    , serviceSectionActions :: [Action]
    }
    deriving (Eq, Show, Generic)

-- | The complete IR document serialized as @intent.json@ for each generation.
data IRDocument = IRDocument
    { irVersion :: IRVersion
    , irProfiles :: [ProfileSection]
    , irServices :: [ServiceSection]
    }
    deriving (Eq, Show, Generic)

dropFieldPrefix :: String -> String -> String
dropFieldPrefix prefix s = case stripPrefix prefix s of
    Just (c : cs) -> toLower c : cs
    Just [] -> error $ "dropFieldPrefix: empty field name after stripping " <> show prefix
    Nothing -> error $ "dropFieldPrefix: " <> show s <> " does not start with " <> show prefix

instance ToJSON IRVersion where
    toJSON (IRVersion n) = toJSON n

instance FromJSON IRVersion where
    parseJSON = fmap IRVersion . parseJSON

instance ToJSON ProfileName where
    toJSON (ProfileName t) = toJSON t

instance FromJSON ProfileName where
    parseJSON value = do
        name <- parseJSON value
        either (fail . show) pure (mkProfileName name)

instance ToJSON ServiceSectionName where
    toJSON (ServiceSectionName t) = toJSON t

instance FromJSON ServiceSectionName where
    parseJSON value = do
        name <- parseJSON value
        either (fail . show) pure (mkServiceSectionName name)

profileOptions :: Options
profileOptions =
    defaultOptions
        { fieldLabelModifier = dropFieldPrefix "profile"
        , omitNothingFields = True
        }

instance ToJSON ProfileSection where
    toJSON = genericToJSON profileOptions

instance FromJSON ProfileSection where
    parseJSON = genericParseJSON profileOptions

serviceSectionOptions :: Options
serviceSectionOptions =
    defaultOptions
        { fieldLabelModifier = dropFieldPrefix "serviceSection"
        , omitNothingFields = True
        }

instance ToJSON ServiceSection where
    toJSON = genericToJSON serviceSectionOptions

instance FromJSON ServiceSection where
    parseJSON = genericParseJSON serviceSectionOptions

irDocumentOptions :: Options
irDocumentOptions =
    defaultOptions{fieldLabelModifier = dropFieldPrefix "ir"}

instance ToJSON IRDocument where
    toJSON = genericToJSON irDocumentOptions

instance FromJSON IRDocument where
    parseJSON = genericParseJSON irDocumentOptions
