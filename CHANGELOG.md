# Changelog

## Unreleased

- `IR.Domain.Service`: `mkServiceName :: Text -> Either DomainError ServiceName` and `serviceNameText :: ServiceName -> Text` — consistent with `mkProcessName`/`processNameText`; empty names are rejected with `EmptyName`.
- `IR.Domain.Service`: `FromJSON Action` now validates service names via `mkServiceName`; empty names produce a parse failure instead of `Just (Disable (ServiceName ""))`.

- Initial release: IR types extracted from FrogOS DSL.
