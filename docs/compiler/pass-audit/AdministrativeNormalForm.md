# AdministrativeNormalForm

## Purpose

Rewrite every kernel-IR expression into **administrative normal form (ANF)**: every
*operand* is atomic (`EVar`, `ECon`, `ELit`, `ENil`). This is the ninth and final pass of
the kernel normalization pipeline, and the invariant that `Coal.Kernel.LLVM.Codegen` relies
on.

---

## Location

```text
src/Coal/Kernel/Pipeline/Pass/AdministrativeNormalForm.hs
```

Invariant checker: `src/Coal/Kernel/Pipeline/Invariant/AdministrativeNormalForm.hs`.

---

## Summary

`administrativeNormalForm` maps over `moduleObjects`, normalizing the body of each
`DFunction` and `DConstant` (other objects contain no expressions). A non-atomic
sub-expression is extracted into a fresh `let anf.<n> = …` binding at its point of use.
Control flow (`EIf`, `ECase`) is kept at the root of a tail position, a `let`-RHS, or an
`ECall` argument, and bound to a fresh variable everywhere else.

It runs inside `KernelCodegen`, after the kernel module has been translated from the
surface language and before LLVM IR is emitted.

---

## Input

- **AST representation**: `Module Kernel.Type`
- **Required invariants** (established by passes 1-8): local names unique; no nested
  lambdas; constructors saturated; lambdas lifted; top-level functions normalized and
  result-saturated; `&&`/`||` translated; no trivial alias `let`s.

---

## Output

- **Resulting AST**: `Module Kernel.Type` with:
  - every operand atomic, with the single exception of `ECall` (see below);
  - fresh `let anf.<n>` bindings for otherwise-non-atomic operands;
  - `DData` / `DExternal` objects and `moduleName` / `moduleImports` unchanged.

---

## Detailed Behavior

### Positions

The traversal `anfExpr` is parameterized by the `Position` of the expression:

| Position | Where | Requirement |
|---|---|---|
| `InTail` | `DFunction`/`DConstant` body; `if` branch; `case` clause body reached from a tail position | result returned in place |
| `InBinding` | RHS of a `let` binding | result returned in place |
| `InOperand` | application function & arguments; operator inputs; record field/row; `case` scrutinee; `if` condition; `ECall` arguments & continuation | result must be atomic |

`anfTail`, `anfLetRhs` and `anfValue` are thin wrappers over `anfExpr` for the three
positions. "Atomic" means one of:

```text
EVar  |  ECon  |  ELit  |  ENil
```

### Atomization of operands

A non-atomic operand is bound to a fresh variable at its point of use:

```text
f(if c then a else b)   ==>   let anf.0 = if c then a else b in f(anf.0)
```

The pass deliberately does **not** float the surrounding computation into the branches
(`if c then f(a) else f(b)`): that would duplicate the continuation once per branch and
blow up quadratically for nested control flow. Binding at the point of use keeps the size
of the output linear in the size of the input.

Bindings introduced while normalizing an *operand* are hoisted so that they enclose the
whole expression; bindings introduced while normalizing a *branch, clause body or lambda
body* stay inside it.

### `ECall` is never bound

`ECall` (`#{name}(args)(k)`) is the continuation-passing foreign-call form. It is the only
non-atomic form the pass leaves in operand position:

```text
f(#ext(x)(k))   ==>   f(#ext(x)(k))      -- NOT bound to a fresh variable
```

Codegen compiles an `ECall` by calling the foreign function and applying its continuation
(`Coal.Kernel.LLVM.Codegen.irValue`; in tail position `irTailECall`, a true tail call, so
that CPS loops do not grow the C stack). Binding the call to a fresh variable would change
that behaviour, so it is passed through unchanged. Its `args` and `k` are still atomized.

### Bookkeeping

- Fresh bindings are named `anf.<n>` using the pipeline's fresh-name counter
  (`Coal.Kernel.Pipeline.freshName`).
- A normalized `let` is re-nested one binding at a time (`anfLetSeq` / `wrapOne`).
- A normalized `case` keeps its declared result type in `InTail` and re-derives it from the
  first clause body in `InBinding`/`InOperand` (`caseResultType`); the two coincide for
  well-typed input.

---

## Compiler Interactions

- **Earlier passes this relies on**: `localNameCanonicalization`, `lambdaFlattening`,
  `constructorSaturation`, `lambdaLifting`, `topLevelFunctionNormalization`,
  `functionResultsSaturation`, `logicalOperatorTranslation`, `letBindingSimplification`
  (i.e. the structural, functional and remaining control-flow passes).
- **Later passes that rely on this pass**: `KernelCodegen` (`Coal.Kernel.LLVM.Codegen`),
  which assumes atomic operands (`irValue`, `irTail`, `irCase`).

---

## Side Effects

- **Modifies compiler state**: advances the fresh-name counter (allocates `anf.<n>` names).
- **Generates diagnostics**: No.
- **Performs IO**: No.
- **Errors**: never raises a `PipelineError`; every rewrite is total.
