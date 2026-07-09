# PhaseTranslation

## Purpose

Normalize, desugar, and compile patterns and expressions in the type-checked AST
into a form suitable for kernel IR translation. Runs per-module (via `mapPass`)
after the type-checking phase.

## Passes Executed

1. **NormalizeAST** — normalize types/expressions via `normalizeObject`
2. **DesugarPatterns** — desugar complex patterns into simple variables with match
3. **ExpandGuards** — expand guard expressions into if-then-else chains
4. **ExpandOrPatterns** — expand or-patterns into separate clauses
5. **CheckPatternAnomalies** — exhaustiveness checking for match expressions
6. **ExpandRecordPatterns** — desugar record patterns into field selects
7. **ExpandAsPatterns** — expand `as` patterns into match+let bindings
8. **ExpandIntegerLiteralPatterns** — expand integer literal patterns into equality guards
9. **CompileMatchExpressions** — compile match expressions into decision trees
10. **InsertDictionaries** — insert trait dictionaries (dictionary-passing style)
11. **CompileNats** — compile nat types to int32-backed representation
12. **DetectCallCycles** — detect explicit recursion cycles (currently commented out)
13. **DenormalizeAST** — apply `denormalizeObject` reverse transformation
14. **CheckTraitAnnotations** — verify trait annotations cover inferred constraints

## Execution Order

```
NormalizeAST
  >-> DesugarPatterns
  >-> ExpandGuards
  >-> ExpandOrPatterns
  >-> CheckPatternAnomalies
  >-> ExpandRecordPatterns
  >-> ExpandAsPatterns
  >-> ExpandIntegerLiteralPatterns
  >-> CompileMatchExpressions
  >-> InsertDictionaries
  >-> CompileNats
  >-> (DetectCallCycles -- currently skipped)
  >-> DenormalizeAST
  >-> CheckTraitAnnotations
```

(Debug artifact generation interleaved between each pass, omitted here for clarity.)

## Inputs

- `Module Metadata Kind IndexedType` — per-module, fully type-checked

## Outputs

- `Module Metadata Kind IndexedType` — per-module, with patterns desugared,
  match expressions compiled to decision trees, trait dictionaries inserted,
  nat types compiled to internal representation, and trait annotations verified

## Invariants Established by the Phase

- Complex patterns are desugared to simple variables with explicit match
- Guard expressions are expanded into if-then-else
- Or-patterns are expanded into separate clauses
- Pattern matching is proved exhaustive
- Record patterns are desugared into field select operations
- As-patterns are expanded into match+let bindings
- Integer literal patterns are expanded into equality guards
- Match expressions are compiled to decision trees (`ECompiledMatch`)
- Trait dictionaries are inserted at all call sites
- Nat types converted to internal `$Nat` representation
- Trait annotations verified against inferred constraints

## Invariants Expected by Later Phases

The lowering phase expects:
- No complex patterns (all desugared to simple variables)
- Match expressions in compiled decision tree form
- Trait dictionaries resolved and inserted
- Nat types in internal form