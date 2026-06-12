# Changelog

## Unreleased

- `IR.Domain.Network`: `Resource` — new sum type with `PortResource Port` constructor. `Fallback` policy constructor changed from `Fallback Port [Port]` to `Fallback Resource [Resource]`; `fallbackPolicy` signature updated accordingly. JSON shape changed: `"target"`/`"strategy"` keys replaced by `"primary"`/`"alternatives"` with `Resource` objects (`{"type":"port","value":N}`).
- `IR.Domain.Service`: `Priority` newtype with `mkPriority :: Int -> Either DomainError Priority` (range [0, 100], returns `InvalidPercent` on failure) and `priorityInt :: Priority -> Int`. `Enable`/`Disable` constructors replaced by `EnableWithPriority ServiceName Priority` and `DisableWithPriority ServiceName Priority`. JSON type strings `"service_enable"` / `"service_disable"` remain valid as legacy aliases that decode to priority 100.
- `IR.Domain.Service`: `mkServiceName :: Text -> Either DomainError ServiceName` and `serviceNameText :: ServiceName -> Text` — consistent with `mkProcessName`/`processNameText`; empty names are rejected with `EmptyName`.
- `IR.Domain.Service`: `FromJSON Action` now validates service names via `mkServiceName`; empty names produce a parse failure instead of `Just (Disable (ServiceName ""))`.
- `IR.Domain.Service`: `ServiceBaseState` — new sum type (`Enabled | Disabled`) describing the state a managed service returns to when no profile acts on it. JSON: `"enabled"` / `"disabled"`.
- `IR.Types`: `ServiceSection` gains `serviceSectionBaseState :: Maybe ServiceBaseState` (JSON key `baseState`, omitted when absent). Legacy documents without the key still parse; no `IRVersion` bump.

- Initial release: IR types extracted from FrogOS DSL.
