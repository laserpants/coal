# DetectCallCycles

## Purpose

Detect call cycles that would lead to unbounded recursion, while allowing
recursion that is purely structural (through fold @-patterns).

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/DetectCallCycles.hs
```

---

## Summary

The pass runs in the translation pipeline between CompileNats and
DenormalizeAST (`>-> passDetectCallCycles` in `PhaseTranslation.hs`). It
builds an edge-typed call graph over the module's definitions and rejects every
call cycle that contains at least one ordinary call edge. There are two kinds
of call edges:

1. **Ordinary calls** — a reference to another definition from an expression
   body (function body, let body, or fold clause body). Extracted by free
   variable analysis.
2. **Structural recursion calls** — references arising from `PNamedFold`
   @-patterns in a top-level fold's clause patterns (e.g.
   `Array(encode_array(@vals))`). The invoked fold is applied to a subterm
   bound by destructuring, so recursion through such calls terminates.

Cycle classification:

- Cycles consisting only of ordinary call edges: **rejected**.
- Cycles consisting only of structural recursion call edges: **allowed** —
  this is the fold recursion scheme (e.g. `encode_value`, `encode_array` and
  `encode_object` mutually recursing through @-patterns).
- Mixed cycles (both kinds): **rejected** — an ordinary call inside the cycle
  is unbounded regardless of the structural calls around it.

---

## Algorithm

1. `buildDependencyGraph` produces, for every function/let/instance-method
   definition, the list of call edges to other definitions of the same module,
   each tagged with its kind. Top-level-fold-derived `DLet` nodes use the
   dependency maps recorded at PrepareBuild time (`foldExprDeps` for ordinary
   edges, `foldPatternDeps` for structural recursion edges) instead of the
   expanded body's free variables, since the calls generated from @-patterns
   are not free variables of the source body.
2. `topoSortDefs` computes strongly connected components over all edges (both
   kinds). An SCC is reported (as a `CallCycle` error) if and only if it
   contains an ordinary call edge whose endpoints both lie inside the SCC:
   - every edge internal to an SCC lies on a cycle, so it witnesses a cycle
     containing an ordinary call;
   - every cycle lies entirely within one SCC, so mixed cycles always
     manifest as an internal ordinary edge;
   - cross-SCC ordinary edges (e.g. a driver function calling into a fold) are
     never part of a cycle and are ignored.

Detected cycles abort compilation with `CallCycleError`.

---

## Dependencies

- **Earlier passes**: PrepareBuild (records `buildFoldExprDeps` and
  `buildFoldPatternDeps`), InsertDictionaries, CompileNats
- **Later passes that would rely on this**: DenormalizeAST

---

## Notes

Fold @-pattern references that are not calls — bare `@x` binders — generate no
edge: only `f(@x)`-style `PNamedFold` patterns are structural recursion calls.
A fold whose clause body explicitly invokes itself (e.g. `sum(rest)`) thereby
creates an ordinary self-edge, and any cycle through it is rejected; the
structural style is to call in the pattern instead (e.g. `sum(@rest)`).
