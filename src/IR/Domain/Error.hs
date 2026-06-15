{-# LANGUAGE OverloadedStrings #-}

-- | Shared errors produced while constructing domain values.
module IR.Domain.Error (
    DomainError (..),
    renderDomainError,
    mkThreshold,
    mkPercent,
    mkName,
    mkInterval,
) where

import Data.Text (Text)
import qualified Data.Text as T

data DomainError
    = EmptyName
    | EmptyList Text
    | InvalidInterval Int
    | InvalidThreshold Double
    | InvalidPercent Int
    | StrategyConflict
    deriving (Eq, Show)

renderDomainError :: DomainError -> Text
renderDomainError EmptyName = "name must not be empty"
renderDomainError (EmptyList what) = what <> " list must not be empty"
renderDomainError (InvalidInterval ms) =
    "interval must be positive, got " <> T.pack (show ms) <> " ms"
renderDomainError (InvalidThreshold t) =
    "threshold must be between 0.0 and 1.0, got " <> T.pack (show t)
renderDomainError (InvalidPercent p) =
    "percent must be between 0 and 100, got " <> T.pack (show p)
renderDomainError StrategyConflict = "observation strategy was already set"

mkThreshold :: (Double -> a) -> Double -> Either DomainError a
mkThreshold ctor t
    | t >= 0.0 && t <= 1.0 = Right (ctor t)
    | otherwise = Left (InvalidThreshold t)

mkPercent :: (Int -> a) -> Int -> Either DomainError a
mkPercent ctor p
    | p >= 0 && p <= 100 = Right (ctor p)
    | otherwise = Left (InvalidPercent p)

mkName :: (Text -> a) -> Text -> Either DomainError a
mkName ctor n
    | n == mempty = Left EmptyName
    | otherwise = Right (ctor n)

mkInterval :: (Int -> a) -> Int -> Either DomainError a
mkInterval ctor ms
    | ms <= 0 = Left (InvalidInterval ms)
    | otherwise = Right (ctor ms)
