# TypeInference

## Purpose

Run bidirectional type inference using constraint generation and solving. Performs both kind inference and type inference, annotating the AST with `IndexedType` throughout.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/TypeInference.hs
```

---

## Summary

The inference process is:
1. **Kind inference** (`inferKinds`): generate kind constraints, solve them, apply the substitution
2. **Type inference** (`inferTypes`): assign type indexes, generate type constraints, solve incrementally, verify assumptions, apply final substitution and normalize
3. **Replace placeholders** (`replacePlaceholders`): update build entries from `NPlaceholder` to `NName` with inferred schemes

Transforms `Module Metadata Kind ()` → `Module Metadata Kind IndexedType`.

---

## Input

- **AST representation**: `Module Metadata Kind ()` — kind-annotated but no type indexes
- **Required invariants**: Lambda-match expanded, build prepared

---

## Output

- **Resulting AST**: `Module Metadata Kind IndexedType` — fully type-annotated
- **Established invariants**: Every expression and pattern has an `IndexedType`; all type constraints solved; name store contains inferred schemes

---

## Detailed Behavior

### `runTypeInference`

1. `inferKinds` — generates kind constraints, solves them via `kindUnifierMonad . solveKindConstraints`, applies the substitution
2. `inferTypes` — see below
3. `replacePlaceholders` — traverses the name store and replaces `NPlaceholder` entries with `NName` entries

### `inferKinds`

- Calls `generateKindConstraints m` which traverses the module generating kind constraints
- Runs `solveKindConstraints` inside `kindUnifierMonad`
- Applies the kind substitution to the name store and module

### `inferTypes`

1. `assignTypeIndices` — uses the supply monad to assign unique `TypeIndex` values throughout the module
2. For each definition, calls `generateConstraints` then `solveT` to solve incrementally
3. Calls `storeDefinitionType` which stores inferred types via `define` (for functions/lets) and `define` with `instanceLabel` (for trait instance members)
4. Verifies all assumptions are satisfied (checks each assumption name against the name store; if missing, reports `NameNotInScope`; if present, generates an `Explicit` constraint to unify the assumed type with the inferred type)
5. Performs a final solve and normalization, applying `rowNormalize` for row types

### `storeDefinitionType`

For `DFunction` and `DLet`: stores the type via `define`. For `DInstance`: uses `instanceLabel` to construct the qualified instance member name and stores each implementation's type.

---

## Analysis

- **Constraint generation**: Uses `generateConstraints` from `Coal.Compiler.TypeInference`
- **Constraint solving**: Incremental via `solveT` from `Coal.Compiler.TypeInference`
- **Kind solving**: Via `solveKindConstraints` from `Coal.TypeSystem.Kind.Constraint.Solver`
- **Substitutions**: Applied via `apply` from `Coal.TypeSystem.Substitution`
- **Environment handling**: Reads/writes `compilerNameStore`, `compilerAssumptions`, `compilerKindConstraints`, `compilerSubstitution`, `compilerSupply`

---

## Performance notes

Type inference is the most expensive phase for definition-heavy programs, and its
cost is dominated by `solveT` (constraint solving), not by `inferKinds`, the
substitution application to the finished module, or the debug dumps.

Solving happens **per definition** (`inferTypes` calls `generateConstraints` then
`solveT` for each definition in turn). Consequently a module whose constraints are
spread over many small definitions is cheap, whereas a single definition that
carries thousands of constraints (e.g. one large list literal of assertions) is
expensive: the solver's work grows super-linearly in the number of constraints of
that *one* definition, not in module size.

The solver (`Coal.TypeSystem.Constraint.Solver`) historically re-applied every
discovered substitution to the *entire* remaining constraint set and composed
substitutions left-nested, and `apply` used generic uniplate traversals. Two
compiler-level changes removed most of that cost while keeping the solve order —
and therefore the output — byte-for-byte identical:

1. **Dirty-set substitution application.** Each constraint is wrapped in a
   `SolverEntry` that caches its free type-index set and its active set (the exact
   `HasActive` value). A new substitution is applied only to the entries that
   mention one of the bound variables; constraints that avoid the bound variables
   are provably unaffected, and `isSolvable`'s active-set query is answered from
   the cached sets instead of a fresh generic traversal of every constraint.
2. **Hand-written traversals.** `apply` for `IndexedType` (`applyIndexedType`) and
   `typeIndexesIn` for `IndexedType`/`Row` are explicit recursions over the type
   constructors instead of `transform . applyT` / `Set.fromList . universeBi`,
   which used to rebuild and scan every nested `Data` value (kinds, names, rows).

Measured on `test/Coal/examples/435` (40 source modules including builtins). The
per-definition figure comes from the `TypeInference__*` dump timestamps, the phase
figure from the compiler's own phase timings:

| | `passTypeInference`, `Data.VariantSpec` | type-checking phase, all 40 modules |
|---|---|---|
| before | 11.66 s (60.8 s in the original report's environment) | 18.04 s |
| after (both changes) | 1.13 s | 5.97 s |

That is 10.4× for the hot definition and 3.0× for the phase. The 60.8 s / 11.66 s
spread for identical pre-change code is environmental (the original run shared the
machine with a build); the before/after pair above was measured back to back under
the same conditions. The remaining 5.97 s is spread evenly over the other 39 modules
(worst module 0.34 s), i.e. the single-definition hot spot is gone. Splitting the
large definition into several smaller ones is still the cheapest way to avoid the
remaining super-linear terms, but it is no longer necessary for this program.

Validation: the `.debug` artifact tree (every pass dump, including inferred and
substituted types) is byte-identical to the pre-change baseline, and the compiled
example's own suite reports all 200 tests passing.

Not addressed (no longer on the critical path at this scale): `unifyAll` re-applies
a substitution per element of its argument list, and constraint *generation*
accumulates output through a left-nested writer list. Both are super-linear, but
their contribution is small once the two changes above are in place.

---

## Compiler Interactions

- **Earlier passes this relies on**: ExpandLambdaMatchExpressions, PrepareBuild, KindIndexing
- **Later passes that rely on this pass**: ReportTypeErrors

---

## Important Data Structures

- `IndexedType` = `Type TypeIndex Kind` — types with numeric indexes
- `IndexedScheme` = `Scheme TypeIndex Kind IndexedType`
- `Constraint` — type constraints including `Explicit`
- `Assumption` — name-type pairs that must be satisfied
- `InferenceRule` — rules generated during constraint generation

---

## Side Effects

- **Generates diagnostics**: `NameNotInScope` for unresolved assumptions; `KindError` for kind inference failures
- **Modifies compiler state**: Updates `compilerNameStore`, `compilerAssumptions`, `compilerKindConstraints`, `compilerSubstitution`, `compilerSupply`
- **Creates fresh names**: Type indexes via the supply monad