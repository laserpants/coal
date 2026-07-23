# DetectCallCycles

## Purpose

Detect explicit recursion cycles (mutual recursion between function calls).

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/DetectCallCycles.hs
```

---

## Summary

This pass is **currently commented out** in the translation pipeline. The file
exists on disk but the pass is not executed. The `>-> passDetectCallCycles` line
in `phaseTranslation` is commented out, and debug artifact generation runs
against placeholder output.

---

## Dependencies

- **Earlier passes**: CompileNats (when active)
- **Later passes that would rely on this**: DenormalizeAST

---

## Notes

The pass appears to be a work in progress. When activated, it would likely
detect cycles in explicit recursive function call graphs (distinct from the
structural recursion handled by folds).