# DesugarPatterns

## Purpose

Desugar complex patterns into simple variable patterns with explicit match expressions.

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/DesugarPatterns.hs
```

## Summary

Transforms complex patterns in let bindings, lambda parameters, and function parameters
into simple variable patterns, extracting the pattern matching logic into explicit
`EMatch` expressions. Uses a `PatternContext` typeclass for recursive traversal.

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: NormalizeAST completed

## Output

- Same type with complex patterns desugared

## Detailed Behavior

### Pattern desugaring

For non-trivial patterns (anything other than `PVariable` or annotated `PVariable`):
1. Generates a fresh variable name `v.N`
2. Records the `(name, pattern)` pair via `tellPatterns`
3. Returns a `PVariable` with the fresh name

### Expression desugaring

For `ELet`, `ERecursiveLet`, `ELambda`: desugars all patterns, then wraps
accumulated pattern bindings into `EMatch` expressions via `unrollMatch`.

For `BFunction`: desugars into `BPattern` with a lambda body, then desugars
that pattern.

### `unrollMatch`

Creates an `EMatch` node with the fresh variable as the scrutinee and the
original pattern as the match clause.

## Transformation Rules

```
let (x, y) = tuple in body
```
becomes:
```
let v.0 = tuple in match(v.0) { | (x, y) => body }
```

## Compiler Interactions

- **Earlier passes this relies on**: NormalizeAST
- **Later passes that rely on this pass**: ExpandGuards

## Side Effects

- **Creates fresh names**: `v.N` via the supply monad
- **Generates diagnostics**: No