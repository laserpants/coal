# ExpandAsPatterns

## Purpose

Expand `as` patterns (`p as name`) into explicit match bindings.

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/ExpandAsPatterns.hs
```

## Summary

Transforms `PAs` patterns in match clauses into nested match expressions that bind the
variable. An `as` pattern `PAs name p` becomes `PVariable name` with the original
pattern checked in a nested match expression. Uses the `Writer` monad to collect
`(label, pattern)` pairs during traversal.

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Record patterns expanded

## Output

- Same type with all `PAs` patterns eliminated

## Detailed Behavior

### `collectAsPatterns`

For `PAs a ll p`:
1. Writes `(ll, p)` to the writer log
2. Returns `PVariable a ll` (the `as` variable becomes a simple variable)

### `expandClause`

For each clause in a match expression:
1. Runs `transformM collectAsPatterns` on the pattern to collect all `as`-patterns
2. For each collected pair, wraps the clause body in a nested `EMatch`:
   ```
   EMatch mempty t (EVariable mempty ll) (EClause mempty p originalBody :| [])
   ```
3. The final clause uses the transformed pattern (with `as`-patterns replaced by variables)

## Transformation Rules

```
match(x) {
  | (a, b) as pair => pair
}
```
becomes approximately:
```
match(x) {
  | pair => match(pair) { | (a, b) => pair }
}
```

This preserves the original binding (`pair` is still bound) while extracting
the inner pattern matching.

## Compiler Interactions

- **Earlier passes this relies on**: ExpandRecordPatterns
- **Later passes that rely on this pass**: ExpandIntegerLiteralPatterns

## Side Effects

- **Generates diagnostics**: No
- **Modifies compiler state**: No