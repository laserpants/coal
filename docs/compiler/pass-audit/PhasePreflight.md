# PhasePreflight

## Purpose

Validate module structure, detect errors, insert builtins, and prepare modules for type
checking. This phase operates on the entire module collection and performs 10 sequential
passes that establish the structural and naming invariants required for type checking.

## Passes Executed

1. **SortModules** — topological sort, cycle detection, main module check
2. **RefreshCache** — invalidate cached builds with modified dependencies
3. **DetectMisplacedImportStatements** — enforce imports at top of module
4. **InsertBuiltinDefinitions** — inject compiler-provided builtin definitions
5. **DesugarDoNotation** — desugar do-notation into monadic bind operations
6. **DetectAliasCycles** — detect cyclic type alias definitions
7. **DetectShadowing** — detect variable shadowing in nested scopes
8. **DetectDuplicateParams** — detect duplicate parameter names
9. **DetectInvalidExports** — validate export lists against module definitions
10. **DetectMainEntrypointMissing** — verify Main module has a `main` function

## Execution Order

```
SortModules
  >-> RefreshCache
  >-> DetectMisplacedImportStatements
  >-> InsertBuiltinDefinitions
  >-> DesugarDoNotation
  >-> DetectAliasCycles
  >-> DetectShadowing
  >-> DetectDuplicateParams
  >-> DetectInvalidExports
  >-> DetectMainEntrypointMissing
```

## Inputs

- `[BuildEnvelope (Module Metadata () ())]` — parsed, untyped kernel modules
  from the parsing phase

## Outputs

- `[BuildEnvelope (Module Metadata () ())]` — validated modules with builtins
  inserted, do-notation desugared, and structural checks passed.

## Invariants Established by the Phase

- Modules are topologically sorted (dependencies before dependents)
- No cyclic imports exist
- A `Main` module exists with a `main` function
- All imports are at the top of each module
- Builtin definitions are inserted into all modules
- Do-notation has been desugared into explicit bind operations
- No type alias cycles exist
- No variable shadowing occurs
- No duplicate parameters exist in any definition
- All exported names exist in the module
- Cache entries reflect the current dependency state

## Invariants Expected by Later Phases

The type checking phase expects:
- All modules in correct dependency order
- No structural errors (cycles, shadowing, duplicates, invalid exports)
- Builtins available for name resolution
- No do-notation remaining (all desugared to explicit binds)
- Main module exists with entry point