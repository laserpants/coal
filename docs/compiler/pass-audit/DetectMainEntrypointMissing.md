# DetectMainEntrypointMissing

## Purpose

Detect when the Main module exists but lacks a `main` function.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/DetectMainEntrypointMissing.hs
```

---

## Summary

Checks the `Main` module specifically. If no definition named `main` exists as a
function, throws `MissingMainEntryPoint` (a fatal error that aborts compilation).

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Invalid exports checked

---

## Output

- **Resulting AST**: Same as input (no transformation)
- **Established invariants**: Main module has a `main` function

---

## Detailed Behavior

### `detectMainEntrypointMissing` (Module instance)

Only checks modules whose path is `Path ["Main"]`. Collects all function
definition names via `functionDefinitions` and checks for `"main"`. If
absent, throws `MissingMainEntryPoint`.

---

## Transformation Rules

None. This is a purely diagnostic pass.

---

## Compiler Interactions

- **Earlier passes this relies on**: DetectInvalidExports
- **Later passes that rely on this pass**: PhaseTypeChecking (needs main entry point)

---

## Side Effects

- **Generates diagnostics**: `MissingMainEntryPoint` (fatal)
- **Modifies compiler state**: No