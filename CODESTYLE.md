# Code Style

## Code Quality

Clean, readable code is the top priority.

- Prefer simple, explicit code over clever abstractions.
- Keep functions small and focused on one responsibility.
- Use names that explain intent without needing extra context.
- Match the style already used in nearby code.
- Avoid unrelated refactors while working on a task.
- Remove dead code instead of leaving it behind.

## Comments

Comments are required when they improve understanding.

- Explain why code exists, not what obvious code does.
- Add comments before non-obvious decisions, invariants, constraints, and trade-offs.
- Keep comments current when changing related code.
- Do not use comments to hide unclear code; make the code clearer first.
- Avoid noisy comments that repeat names, types, or control flow.

## Haskell

- Prefer total functions and explicit error handling.
- Keep public APIs small and intentional.
- Use types to document domain concepts.
- Avoid partial functions unless the invariant is local, obvious, and documented.
- Keep formatting stable through the project formatter.
- All data type fields must be strict. The library enables `StrictData` globally via `default-extensions` in `DSL.cabal` — do not add individual `!` annotations, they are redundant.
- Do not use `length` to compute offsets or check sizes; prefer `Data.List.stripPrefix`, pattern matching, or `null`. `length` traverses the entire spine and hangs on infinite structures (STAN-0103).
- Do not use `_` as a catch-all in `case` on sum types. Use a named wildcard like `_unknownFoo` to make the intent explicit and aid future maintainability (STAN-0213).

## Nix

- Keep flakes reproducible and locked.
- Prefer project-local configuration over implicit environment assumptions.
- Keep package names aligned with the Cabal package.
- Run flake checks after changing Nix files.

---

## Git

### Branch names

```
<type>/<short-description>
```

| Type | When to use |
|------|-------------|
| `feat` | new user-visible functionality |
| `fix` | bug fix |
| `test` | tests only, no production code changes |
| `refactor` | restructuring without behaviour change |
| `docs` | documentation only |
| `chore` | tooling, CI, build, dependencies |

Use lowercase kebab-case for the description. Keep it short (3–5 words).

Examples: `feat/network-fallback-policy`, `fix/empty-name-validation`, `test/issue-10-suite`.

### Commit messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <short summary>

<optional body — why, not what>
```

- **type** — same set as branch types above (`feat`, `fix`, `test`, `refactor`, `docs`, `chore`)
- **scope** — the affected module or area in parentheses, e.g. `(validate)`, `(network)`, `(ci)`; omit when the change is cross-cutting
- **summary** — imperative mood, lowercase, no trailing period, ≤72 characters
- **body** — wrap at 72 characters; explain motivation and context, not the diff

---

## Testing

### TDD Workflow

Write the test before the implementation.

1. Write a failing test that names the expected behaviour.
2. Write the minimal implementation that makes it pass.
3. Refactor, keeping all tests green.

Do not write implementation code speculatively. If you cannot express the expected behaviour as a test first, clarify the requirement.

### Pure Function Tests

Pure functions get unit tests that call the function directly and assert the result.
No setup, no mocks, no IO. If a test requires more than calling the function and comparing the output, the function is probably not pure enough.

```haskell
testCase "via attaches poll strategy to processRunning" $
    processRunning "steam" `via` poll 500
        @?= ProcessCondition
            (Process.ProcessRunning (requireRight (Process.mkProcessName "steam"))
                                    (Just (Process.Poll (Process.IntervalMs 500))))
```

Group related cases under a `testGroup` so failures are easy to locate.

### Property Tests

Property tests go in a `testGroup "properties"` block at the end of each spec module.
Use `testProperty` from `Test.Tasty.QuickCheck`.

A good property names an invariant, not a computation:

```haskell
testProperty "via replaces strategy, not condition" $
    \name s1 s2 ->
        (processRunning name `via` s1 `via` s2)
            === (processRunning name `via` (s2 :: ObserveStrategy))
```

Prefer `===` over `==` so QuickCheck prints both sides on failure.
Use `.&&.` to combine multiple assertions in one property without splitting into separate runs.

### Shrinking (Required for Every `Arbitrary` Instance)

Every `Arbitrary` instance **must** implement `shrink`. Without it, a failure on a
50-element list stays a 50-element list; with it, QuickCheck collapses the example to the 1–2
elements that actually trigger the bug.

Rules:
- Shrink each field independently, keeping the constructor fixed. Switching constructors changes
  the counterexample structure and hides the real cause.
- Filter values that would violate invariants (e.g. negative intervals, empty names).
- When a constructor is opaque (built via a smart constructor), shrink the underlying primitive
  and reconstruct, or return `[]` if no meaningful reduction is possible.

```haskell
instance Arbitrary Process.ProcessName where
    arbitrary = requireRight . Process.mkProcessName . T.pack <$> listOf1 arbitrary
    shrink name =
        [ candidate
        | text <- shrink (Process.processNameText name)
        , Right candidate <- [Process.mkProcessName text]
        ]

-- sum type: shrink each constructor independently
instance Arbitrary Network.Policy where
    arbitrary =
        oneof
            [ Network.AllowPorts <$> arbitrary
            , Network.Fallback <$> arbitrary <*> arbitrary
            ]
    shrink (Network.AllowPorts ps) = Network.AllowPorts <$> shrink ps
    shrink (Network.Fallback pri alts) =
        [Network.Fallback pri' alts | pri' <- shrink pri]
            <> [Network.Fallback pri alts' | alts' <- shrink alts]
```

`Arbitrary` instances live in `test/DSL/Arbitraries.hs` as orphans. Keep them there; do not
scatter them across spec files.

### Test Fixtures

Shared concrete values (fixed `IRDocument`s, named examples) belong in `test/Fixtures.hs`.
Fixtures are for golden tests and round-trip tests, not for property tests — use `Arbitrary`
for those.

### Golden Tests

Serialization output is verified with `goldenVsString`. The expected files live under
`test/fixtures/`. Update the golden files intentionally; never delete them to silence a failure.
