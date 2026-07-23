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