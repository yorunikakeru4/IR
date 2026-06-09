-- | Shared errors produced while constructing domain values.
module IR.Domain.Error (
    DomainError (..),
    mkThreshold,
    mkPercent,
    mkName,
    mkInterval,
) where

import Data.Text (Text)

data DomainError
    = EmptyName
    | EmptyList Text
    | InvalidInterval Int
    | InvalidThreshold Double
    | InvalidPercent Int
    deriving (Eq, Show)

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
