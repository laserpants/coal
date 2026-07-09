# Investigate runtime crash

When a compiled Coal program crashes at runtime (segfault, abort, or incorrect
output), follow this workflow to isolate the cause.

> ⚠️ **Always rebuild after source changes.** Running a bare `coal` binary
> tests the *previously installed* version, not your latest edits. Use
> `stack run` (or `cabal run`) to guarantee a rebuild from source before
> every pipeline-level test. A `stack run` invocation builds the compiler,
> then compiles and links the Coal program under test, so the entire
> toolchain reflects your current source tree.
>
> ```bash
> # ❌ Stale — tests the previously installed binary, not your edits:
> coal compile -I. Main.coal -o /tmp/test
>
> # ✅ Correct — rebuilds compiler + runtime before testing:
> stack run coal -- compile -I. Main.coal -o /tmp/test && /tmp/test
> ```

## Debugging tiers

Choose the appropriate approach for the stage you are at:

| Tier | Tool | When to use |
|------|------|-------------|
| 1 | **E2E test harness** | Regression checking after a suspected fix; verifying behaviour against known-good examples |
| 2 | **`stack run coal -- compile`** | Full pipeline verification: edit source, recompile, run the binary |
| 3 | **`stack ghci`** | Quick isolation: testing a single pass, inspecting intermediate IR, evaluating small expressions interactively |

---

## Tier 1 — E2E test harness

If the program can be reduced to a small example, add it to the E2E test suite
for repeatable regression testing:

1. Create a new directory `test/examples/NNN/` containing `Main.coal` (and any
   additional modules).
2. Create `test/examples/NNN/.expected` with the expected stdout output (or
   leave empty, and the test will create it on first run).
3. Run the full E2E suite (slow) or a filtered subset:

```bash
# Run all E2E tests (slow — prefer the filtered form below):
stack run test-exe

# Run a single test by example name:
stack run test-exe -- --match "NNN"
```

The E2E harness (`test/E2E/Spec.hs`) runs the full compiler pipeline
in-process via `evalCompilerT` and then executes the resulting binary
(`./dist`). Because `stack run` rebuilds before executing, every run
reflects the current source tree.

---

## Tier 2 — Full pipeline verification

Use `stack run coal -- compile` for end-to-end compilation and execution.
This is appropriate when you have made a source change and want to confirm
the fix produces correct output.

### 1. Reproduce the crash

```bash
stack run coal -- compile -I. Main.coal -o /tmp/test && /tmp/test
```

Note the exact exit code, stderr output, and whether the crash is deterministic.

### 2. Isolate to compiler or runtime

- If the kernel IR looks correct but the program still crashes, the bug is
  likely in the LLVM codegen or the runtime library.
- Compile with debug symbols and run under a debugger (`lldb` / `gdb`) to
  locate the crash site.

### 3. Inspect intermediate representations

Use compiler flags to dump kernel IR at various stages:

```bash
stack run coal -- compile --dump-kernel Main.coal
```

Check the kernel IR after each normalization pass. Verify that:
- The program is in ANF (every non-atomic sub-expression is a let-binding)
- All constructor applications are fully saturated
- No free variables exist that should have been bound
- Lambda expressions have been lifted or flattened

### 4. Reduce to a minimal example

Strip the program down to the smallest possible Coal source that still triggers
the crash. The ideal minimal example is a single function with a single
expression. Once reduced, consider adding it to the E2E test suite (Tier 1).

### 5. Check the runtime library

If the kernel IR looks correct, the bug may be in `runtime/`. Check:
- Are GC allocations using `rt_alloc` / `rt_alloc_atomic` (never `malloc`)?
- Are fixed-width integer types used consistently?
- Is the runtime function signature in `RuntimeDefs.hs` consistent with the
  actual C implementation in `runtime/src/`?

### 6. Add debug instrumentation

Add `printf` calls to the C runtime at the crash site to trace values.
Recompile the runtime with `stack run coal -- compile ...` (re-linking
is automatic).

### 7. Only then propose a fix

Explain which invariant was violated and why the proposed change restores it.

---

## Tier 3 — GHCI interactive debugging

When you need to inspect the output of a specific compiler pass without
running the full pipeline, use `stack ghci`. This is the fastest feedback
loop for isolating which pass introduces incorrect IR.

### Loading the compiler

```haskell
stack ghci
-- Inside GHCI:
:l src/Coal/Compiler.hs
```

### Useful evaluations

```haskell
-- Run the full pipeline on a file (returns Either CompilerFailureMode String):
import Coal.Compiler (pipeline)
import Coal.Compiler.Config (defaultConfig)
import Coal.Compiler.Environment (emptyCompilerEnvironment)
import Coal.Compiler.Pass (runPass)
import Coal.Compiler.State (setConfigC)

evalCompilerT (emptyCompilerEnvironment Nothing) $ do
  setConfigC defaultConfig{configNoCache = True, configSourcePaths = ["test/examples/NNN"]}
  runPass pipeline ["Main.coal"]
```

For inspecting a specific normalization pass, import it directly from
`src/Coal/Kernel/Pipeline/Passes.hs` and apply it to a parsed kernel module
within the `PipelineT` monad.

```haskell
-- Example: inspecting a specific normalization pass
import Coal.Kernel.Pipeline.Passes (administrativeNormalForm)
```

> **Note:** GHCI evaluates Haskell expressions, not Coal programs. Use it
> to test individual compiler passes or to pretty-print intermediate IR.
> Use Tier 2 (`stack run coal -- compile`) when you need to execute compiled
> Coal code.