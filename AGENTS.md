# Project Rules

These rules apply to the whole repository.

See [CODESTYLE.md](CODESTYLE.md) for code quality, comments, Haskell, Nix, and testing conventions.

## Dev

- Don’t make major or fundamental changes without verification or a plan.
- After changes use `nix fmt` for formatting.
- Before committing use `nix flake check --keep-going --print-build-logs` and `cabal test`.
- For checking issues/PRs use `tea` instead of `gh`.
- To view a single issue with body: `tea issues --fields "index,title,body" <number>`
- Don’t commit docs from superpowers

## Changelog

When any public API changes — new types, constructors, functions, removed or renamed exports — add an entry to `CHANGELOG.md` under the current unreleased version before committing.

## Verification

Before finishing code changes, run the narrowest command that proves the change works.

- For Nix changes, run `nix flake check`.
- For formatting changes, run `nix fmt`.
- For Haskell changes, run the relevant Cabal or Nix build/test command.

Report any verification that could not be run.
