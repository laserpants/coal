# ExpandGuards

## Purpose

Expand guard expressions in pattern matching clauses into nested if-then-else expressions.

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/ExpandGuards.hs
```

## Summary

Transforms guarded pattern match clauses into explicit if-then-else chains.
Multi-guard clauses are combined with logical AND. Fallthrough to subsequent
clauses is ensured by placing the remaining match as the else-branch.

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: DesugarPatterns completed

## Output

- Same type with guard expressions eliminated from all clauses

## Detailed Behavior

### `expandExpression`

Only processes `EMatch` nodes that have non-trivial clauses (clauses with guards).
1. Generates a fresh scrutinee name `scr.N`
2. Binds the match expression to this variable via `ELet`
3. Calls `expandClauseGuards` on each clause with the remaining clauses as
   fallback

### `expandClauseGuards`

For a clause with multiple choices (guards):
- Folds from the right: each `CPlain guards expr` becomes
  `if (guards_combined) then expr else <remaining_or_fallback>`
- The final choice's guard becomes unreachable (it's the "otherwise" case)
- Guards within a single choice are combined via `conjunction` (logical AND)

### `conjunction`

Combines two boolean expressions using `EApplication` with the logical AND operator.

## Transformation Rules

```
match(x) {
  | Just(y) when (y > 0) => positive(y)
  | Nothing => Zero
}
```
becomes:
```
let scr = x in match(scr) {
  | Just(y) => if (y > 0) then positive(y) else match(scr) { | Nothing => Zero }
  | Nothing => Zero
}
```

## Compiler Interactions

- **Earlier passes this relies on**: DesugarPatterns
- **Later passes that rely on this pass**: ExpandOrPatterns

## Side Effects

- **Creates fresh names**: `scr.N` via supply monad
- **Generates diagnostics**: No