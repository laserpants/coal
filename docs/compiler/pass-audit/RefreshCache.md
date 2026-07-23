# RefreshCache

## Purpose

Refresh cached build artifacts when their dependencies have been modified, enabling
incremental compilation by reusing cached builds only when dependencies haven't changed.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/RefreshCache.hs
```

---

## Summary

For each `BCached` envelope, checks whether any of its dependencies have been
"touched" (modified) during the current compilation. If so, the cached build is
invalidated and the module is re-parsed from source. If not, the cached build is
preserved.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Assumptions**: Modules are sorted in dependency order
- **Required invariants**: SortModules has already run (dependencies tracked in builds)

---

## Output

- **Resulting AST**: `[BuildEnvelope (Module Metadata () ())]` — with stale
  caches replaced by fresh `BSource` envelopes
- **Established invariants**: All `BCached` envelopes are known to be up-to-date
  relative to their dependencies
- **Guarantees made to later passes**: Subsequent passes can trust that cached
  modules are valid

---

## Detailed Behavior

### `passImpl`

Traverses all envelopes with `traverse refreshCache`.

### `refreshCache`

- `BSource` envelopes pass through unchanged
- `BCached` envelopes: checks `buildDependencies` against `compilerTouched` set.
  If any dependency is touched, calls `compileFromSource` to re-parse; otherwise
  keeps the cached build

### `compileFromSource`

Retrieves the source text from `compilerSources`, re-parses it with
`parseSourceFile`, and returns a fresh `BSource` envelope. Reports parse
errors if the previously-valid source now fails to parse (which indicates
source corruption or a compiler bug).

---

## Transformation Rules

No AST transformations beyond re-parsing from source. The only change is
that `BCached` becomes `BSource` for stale modules.

---

## Analysis

- **Environment handling**: Checks `compilerTouched` set against dependency names
- **I/O**: None (re-parses from in-memory source text)

---

## Compiler Interactions

- **Earlier passes this relies on**: SortModules (needs dependency information),
  Parsing (needs source text in `compilerSources`)
- **Later passes that rely on this pass**: All subsequent passes running on
  the module list

---

## Important Data Structures

- `BuildEnvelope` — `BSource` vs `BCached` distinction
- `CompilerState.computerTouched` — set of module names modified in current build

---

## Side Effects

- **Generates diagnostics**: Parse errors for corrupted cache sources
- **Modifies compiler state**: Marks re-parsed modules as touched
- **Performs IO**: No (works from in-memory state)

---

## Notes

This pass is the key mechanism for incremental compilation: cached builds avoid
re-parsing and re-typechecking modules whose dependencies haven't changed.