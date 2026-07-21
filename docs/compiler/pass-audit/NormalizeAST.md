# NormalizeAST

## Purpose

Apply normalization transformations to the typed AST.

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/NormalizeAST.hs
```

## Summary

Calls `normalizeObject` from `Coal.Language.AST.Normalization`, which performs
type-based normalization on the module. The pass is a pure transformation
(no monadic effects).

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Type checking completed with no errors

## Output

- **Resulting AST**: Same type, with normalized types/expressions

## Detailed Behavior

`passImpl = return . normalizeObject` — applies the `NormalizationContext`'s
`normalizeObject` method, which traverses the module and normalizes types and
expressions into a canonical form.

## Compiler Interactions

- **Earlier passes this relies on**: ReportTypeErrors
- **Later passes that rely on this pass**: DesugarPatterns

## Side Effects

None (pure transformation).