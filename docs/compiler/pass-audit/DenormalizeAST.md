# DenormalizeAST

## Purpose

Apply the reverse normalization transformation to the AST, undoing the effects
of `NormalizeAST`.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/DenormalizeAST.hs
```

---

## Summary

Calls `denormalizeObject` from `Coal.Language.AST.Normalization`, which is the
conceptual inverse of `normalizeObject`. This is a pure transformation with no
monadic effects.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Nats compiled, dictionaries inserted

---

## Output

- Same type, with denormalized types/expressions

---

## Detailed Behavior

`passImpl = return . denormalizeObject` — applies the `NormalizationContext`'s
`denormalizeObject` method, traversing the module and reversing the normalization
that was applied earlier in `NormalizeAST`.

---

## Compiler Interactions

- **Earlier passes this relies on**: CompileNats (and DetectCallCycles, when active)
- **Later passes that rely on this pass**: CheckTraitAnnotations (last pass in
  the translation phase)

---

## Side Effects

None (pure transformation).