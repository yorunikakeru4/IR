# Changelog

## Unreleased

- `IR.Domain.Service`: `Priority` newtype with `mkPriority :: Int -> Either DomainError Priority` (range [0, 100], returns `InvalidPercent` on failure) and `priorityInt :: Priority -> Int`. `Enable`/`Disable` constructors replaced by `EnableWithPriority ServiceName Priority` and `DisableWithPriority ServiceName Priority`. JSON type strings `"service_enable"` / `"service_disable"` remain valid as legacy aliases that decode to priority 100.
- `IR.Domain.Service`: `mkServiceName :: Text -> Either DomainError ServiceName` and `serviceNameText :: ServiceName -> Text` — consistent with `mkProcessName`/`processNameText`; empty names are rejected with `EmptyName`.
- `IR.Domain.Service`: `FromJSON Action` now validates service names via `mkServiceName`; empty names produce a parse failure instead of `Just (Disable (ServiceName ""))`.

- Initial release: IR types extracted from FrogOS DSL.
