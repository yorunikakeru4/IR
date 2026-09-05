# FrogOS IR

Intermediate Representation (IR) types for FrogOS, shared between the DSL
compiler and the Planner. IR is the stable contract: a declarative, versioned
JSON document that fully describes the user's intent.

## Role

```text
configuration.frog -> DSL compiler -> IR JSON -> FrogOS engine
```

The IR sits between the Haskell configuration language and the runtime. It is:

- **Serializable** — emitted as compact JSON by the DSL compiler.
- **Versioned** — `IRVersion` is an incrementing counter; the engine refuses to
  read documents with an unsupported version.
- **Self-contained** — closed sums for `Condition`, `Policy` and `Action`; the
  engine knows every variant up front, so there are no open extension points.

## Domain Areas

The IR is organized into intent areas derived from the DSL:

| Area                     | Purpose                                         |
| ------------------------- | ------------------------------------------------ |
| `profiles`               | Environment profiles and power settings          |
| `modules`                | Enable/disable state for system modules          |
| `network`                | Port allowances and network policy               |
| `process` / `package`    | Process and package-level conditions/actions     |
| `user` / `system`        | Per-user and system-wide operations              |

Each area contains three kinds of nodes:

- **`Condition`** — a boolean trigger ("when is this relevant?").
- **`Policy`** — a rule or constraint with options (e.g. `fallback`).
- **`Action`** — a desired system change (e.g. `setPowerProfile Performance`).

## Example

A minimal IR document (shape only):

```json
{
  "ir_version": 10,
  "profiles": {
    "performance": { "power": { "set_power_profile": { "priority": 100 } } }
  }
}
```

## Module Support

The current IR ships domain support for nginx (including virtual hosts with
per-host policies), PostgreSQL, Forgejo, and virtualization. New areas are
added as new keys at the root of the IR document without changing existing
schema areas.

## Development

```bash
nix develop
cabal test        # Haskell test suite (UsefulHspec)
nix fmt
stan --no-default
nix flake check --keep-going --print-build-logs
```

Project rules live in [AGENTS.md](AGENTS.md), [CLAUDE.md](CLAUDE.md), and
[CODESTYLE.md](CODESTYLE.md). See [CHANGELOG.md](CHANGELOG.md) for the version
history.

## License

LGPL-2.1-only.