# ReportTypeErrors

## Purpose

Collect type errors from both the constraint generation phase and the solver phase,
report them, and abort compilation if any errors were found.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/ReportTypeErrors.hs
```

---

## Summary

Reads `compilerGetConstraintsGenErrorsC` (constraint generation errors),
`compilerGetSolverRuleViolationsC` (solver rule violations), and
`compilerGetKindConstraintsGenErrorsC` (kind-constraint-generation errors), reports
each as a `ConstraintsError`, `SolverError`, or `KindError`, and throws `TypeError`
if any errors exist.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Type inference completed

---

## Output

- **Resulting AST**: Same as input (no transformation, unless errors cause abort)
- **Established invariants**: Compilation only proceeds if zero type errors exist

---

## Detailed Behavior

### `passImpl`

1. Reads constraint generation errors from state
2. Reports each via `tellErrors [ConstraintsError err (errorLocation err)]`
3. Reads solver rule violations from state
4. Reports each via `tellErrors [SolverError err (errorLocation err)]`
5. Reads kind-constraint-generation errors from state (collected by
   `generateKindConstraints`) and reports each via
   `tellErrors [KindError err (ErrorLocation …)]`. These carry no source metadata
   of their own, so they are reported at module granularity. Without this step the
   errors are silently dropped, and e.g. a data constructor mentioning an undefined
   type constructor is accepted.
6. If any errors exist, throws `TypeError`

---

## Compiler Interactions

- **Earlier passes this relies on**: TypeInference
- **Later passes that rely on this pass**: PhaseTranslation (only proceeds if no errors)

---

## Side Effects

- **Generates diagnostics**: `ConstraintsError`, `SolverError`, `KindError`
- **Modifies compiler state**: No