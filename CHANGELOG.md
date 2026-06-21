# Changelog

## Unreleased

- `IR.Domain.Module.Docker`: removed. `DockerConfig` and `StorageDriver` are no longer part of the IR.
- `IR.Domain.Module`: removed `DockerDomain` constructor from `ModuleDomain`.
- `IR.Types`: removed `mmDocker :: [DockerConfig]` field from `ModuleMap`. `emptyModuleMap`, `mmIsEmpty`, `ToJSON`, and `FromJSON` updated accordingly. `ModuleMap` now has three fields: `mmPostgreSQL`, `mmNginx`, `mmForgejo`.

- `IR.Domain.Module`: `EnableModule` and `DisableModule` constructors now carry an `Int` priority field (0–100). JSON gains a `"priority"` key; decoding defaults to `100` when absent for backward compatibility. `currentIRVersion` bumped from 5 to 6.

- `IR.Types`: removed `ServiceSectionName`, `ServiceSection`, `mkServiceSectionName`,
  `serviceSectionNameText`, and `irServices`. `IRDocument` JSON no longer contains
  a `services` key. `currentIRVersion` bumped from 4 to 5.
- `IR.Action`: removed `ServiceAction` and service action decoding. Module lifecycle
  actions are now the only lifecycle action domain.
- `IR.Domain.Service`: removed the service domain module.
- `IR.Domain.Module.PostgreSQL`, `IR.Domain.Module.Nginx`, and `IR.Domain.Module.Forgejo`:
  configs gain module-level `policies :: [Policy]` fields, serialized as optional
  `policies` keys and defaulting to `[]` when absent.
- `IR.Domain.Error`: `InvalidPort Int` for invalid TCP/UDP port values.

- Source modules were flattened by dropping the top-level `IR.` namespace. For example, import `Types` instead of `IR.Types`, and `Domain.Module.Nginx` instead of `IR.Domain.Module.Nginx`.

- `IR.Domain.Module.PostgreSQL`: `PostgreSQLConfig` gains `postgresqlMaxConnections :: Maybe Int` (JSON key `max_connections`, omitted when absent).
- `IR.Domain.Module.Nginx`: `NginxConfig` gains `nginxHttpPort :: Maybe Int`, `nginxHttpsPort :: Maybe Int`, `nginxDomain :: Maybe Text` (JSON keys `http_port`, `https_port`, `domain`, omitted when absent).
- `IR.Domain.Module.Forgejo`: `ForgejoConfig` gains `forgejoHttpPort :: Maybe Int`, `forgejoSshPort :: Maybe Int`, `forgejoDomain :: Maybe Text` (JSON keys `http_port`, `ssh_port`, `domain`, omitted when absent).

- `IR.Domain.Module`: `ModuleDomain` enum (`DockerDomain | PostgreSQLDomain | NginxDomain | ForgejoDomain`), `ModuleName` newtype with `mkModuleName`/`moduleNameText`, `ModuleRef` with `moduleRefDomain`/`moduleRefName`, `LifecycleAction` (`EnableModule | DisableModule | RestartModule ModuleRef`). JSON type strings: `module_enable`, `module_disable`, `module_restart`.
- `IR.Domain.Module.Docker`: `DockerConfig` (singleton, name always `"default"` in JSON) with `dockerEnable`, `dockerRootless`, `dockerStorageDriver :: Maybe StorageDriver`. `StorageDriver` enum (`Overlay2 | VFS | BTRFS`).
- `IR.Domain.Module.PostgreSQL`: `PostgreSQLConfig` with `postgresqlConfigName :: ModuleName`, `postgresqlEnable`, `postgresqlPort :: Maybe Int`, `postgresqlDataDir :: Maybe Text`.
- `IR.Domain.Module.Nginx`: `NginxConfig` with `nginxConfigName :: ModuleName`, `nginxEnable`.
- `IR.Domain.Module.Forgejo`: `ForgejoConfig` with `forgejoConfigName :: ModuleName`, `forgejoEnable`.
- `IR.Action`: `Action` gains `ModuleAction Module.LifecycleAction` constructor with `LiftAction` instance; `ToJSON`/`FromJSON` route `module_enable`, `module_disable`, `module_restart`.
- `IR.Types`: `ModuleMap` record (`mmDocker`, `mmPostgreSQL`, `mmNginx`, `mmForgejo`); `emptyModuleMap`; `mmIsEmpty`. `IRDocument` gains `irModules :: ModuleMap` — omitted from JSON when all domain lists are empty, defaults to `emptyModuleMap` when absent. `currentIRVersion` bumped from 2 to 3.

- `IR.Domain.Package`: new module. `PackageName` newtype with `mkPackageName :: Text -> Either DomainError PackageName` and `packageNameText :: PackageName -> Text`. `ToJSON`/`FromJSON` instances — serialises as a plain JSON string.
- `IR.Types`: `IRDocument` gains `irPackages :: [PackageName]` (global packages, always installed). `ProfileSection` gains `profilePackages :: [PackageName]` (conditional packages, available while the profile is active). `ServiceSection` gains `serviceSectionPackages :: [PackageName]` (packages associated with the service). All three fields are omitted from JSON when empty and default to `[]` when absent — backward compatible with existing documents.

- `IR.Domain.Process`: `AppName` newtype with `mkAppName :: Text -> Either DomainError AppName` and `appNameText :: AppName -> Text` — represents a FrogOS app/package identity.
- `IR.Domain.Process`: `AppRunning AppName (Maybe ObserveStrategy)` — new `Condition` constructor; true while any process belonging to the named FrogOS app is running. JSON type string `"app_running"`.
- `IR.Domain.Process.attachStrategy` — handles the new `AppRunning` constructor.
- `IR.Condition`: `FromJSON` routes `"app_running"` to `ProcessCondition`.

- `IR.Domain.Power`: `PowerProfile` now derives `Ord`.
- `IR.Domain.Power`: `parsePowerProfile :: Text -> Either DomainError PowerProfile` — parse a power profile name; returns `Left (UnknownPowerProfile t)` for unrecognised strings.
- `IR.Domain.Power`: `renderPowerProfile :: PowerProfile -> Text` — inverse of `parsePowerProfile`; returns the canonical lowercase profile name.
- `IR.Domain.Error`: `UnknownPowerProfile Text` — new constructor for unrecognised power profile names.

- `IR.Domain.Service`: `actionName :: Action -> ServiceName` and `actionPriority :: Action -> Priority` — accessors for the common fields of `EnableWithPriority` and `DisableWithPriority`.

- `IR.Domain.Error`: `DuplicateCondition` — a profile body contained more than one `when` condition; `renderDomainError` returns `"a profile may have at most one when condition"`.

- `IR.Domain.Error`: `renderDomainError :: DomainError -> Text` — human-readable message for each error constructor. Used by all `FromJSON` parsers instead of `show` dumps.
- All `FromJSON` instances in `IR.Domain.Network`, `IR.Domain.Process`, `IR.Domain.Service`, `IR.Domain.System`, `IR.ObserveStrategy`, and `IR.Types` now call `renderDomainError` via `fail` instead of `show`. Parse errors are now human-readable.

- `IR.Domain.Error`: `StrategyConflict` — attaching an observation strategy to a condition leaf that already has one now fails instead of silently replacing it.
- `IR.Domain.Process.attachStrategy` and `IR.Domain.System.attachStrategy` — attach an observation strategy within the owning domain module and return `Either DomainError Condition`.
- `IR.Domain.Network`: `Resource` — new sum type with `PortResource Port` constructor. `Fallback` policy constructor changed from `Fallback Port [Port]` to `Fallback Resource [Resource]`; `fallbackPolicy` signature updated accordingly. JSON shape changed: `"target"`/`"strategy"` keys replaced by `"primary"`/`"alternatives"` with `Resource` objects (`{"type":"port","value":N}`).
- `IR.Domain.Service`: `Priority` newtype with `mkPriority :: Int -> Either DomainError Priority` (range [0, 100], returns `InvalidPercent` on failure) and `priorityInt :: Priority -> Int`. `Enable`/`Disable` constructors replaced by `EnableWithPriority ServiceName Priority` and `DisableWithPriority ServiceName Priority`. JSON type strings `"service_enable"` / `"service_disable"` remain valid as legacy aliases that decode to priority 100.
- `IR.Domain.Service`: `mkServiceName :: Text -> Either DomainError ServiceName` and `serviceNameText :: ServiceName -> Text` — consistent with `mkProcessName`/`processNameText`; empty names are rejected with `EmptyName`.
- `IR.Domain.Service`: `FromJSON Action` now validates service names via `mkServiceName`; empty names produce a parse failure instead of `Just (Disable (ServiceName ""))`.
- `IR.Domain.Service`: `ServiceBaseState` — new sum type (`Enabled | Disabled`) describing the state a managed service returns to when no profile acts on it. JSON: `"enabled"` / `"disabled"`.
- `IR.Types`: `ServiceSection` gains `serviceSectionBaseState :: Maybe ServiceBaseState` (JSON key `baseState`, omitted when absent). Legacy documents without the key still parse; no `IRVersion` bump.

- Initial release: IR types extracted from FrogOS DSL.
