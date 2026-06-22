# Project Rules

These rules apply to the whole repository.

See [CODESTYLE.md](CODESTYLE.md) for code quality, comments, Haskell, Nix, and testing conventions.

## Dev

- Don’t make major or fundamental changes without verification or a plan.
- After changes use `nix fmt` for formatting.
- Before committing use `stan --no-default`, `nix flake check --keep-going --print-build-logs` and `cabal test`.
- For checking issues/PRs use `tea` instead of `gh`.
- To view a single issue with body: `tea issues --fields "index,title,body" <number>`
- Don’t commit docs from superpowers

## State-mutating actions carry priority

Any `Action` constructor that mutates system state (enables/disables/restarts a module, sets a power profile, or similar) must carry an `Int` priority field. The field name in JSON is `"priority"`. Decoding must default to `100` when the field is absent. When adding a new state-mutating action: add the `Int` parameter to the constructor, emit `"priority"` in `ToJSON`, and default to `100` in `FromJSON`.

## Changelog

When any public API changes — new types, constructors, functions, removed or renamed exports — add an entry to `CHANGELOG.md` under the current unreleased version before committing.

## Verification

Before finishing code changes, run the narrowest command that proves the change works.

- For Nix changes, run `nix flake check`.
- For formatting changes, run `nix fmt`.
- For Haskell changes, run the relevant Cabal or Nix build/test command.

Report any verification that could not be run.
