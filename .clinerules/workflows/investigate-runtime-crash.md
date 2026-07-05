# Investigate runtime crash

When a compiled Coal program crashes at runtime (segfault, abort, or incorrect
output), follow this workflow to isolate the cause.

## 1. Reproduce the crash

Capture the exact command, input, and output:

```bash
coal compile -I. Main.coal -o /tmp/test && /tmp/test
```

Note: exact exit code, stderr output, and whether the crash is deterministic.

## 2. Isolate to compiler or runtime

- **Compile the same program with the legacy compiler** (if applicable). If the
  legacy-compiled binary works, the bug is in the new pipeline.
- If both compile but crash, compare the LLVM IR or kernel IR output between
  pipelines.

## 3. Inspect intermediate representations

Use compiler flags to dump kernel IR at various stages:

```bash
coal compile --dump-kernel Main.coal
```

Check the kernel IR after each normalization pass. Verify that:
- The program is in ANF (every non-atomic sub-expression is a let-binding)
- All constructor applications are fully saturated
- No free variables exist that should have been bound
- Lambda expressions have been lifted or flattened

## 4. Compare with legacy compiler output

If the new pipeline is suspect, run the legacy pipeline on the same input and
compare the kernel IR. See `workflows/compare-pipelines.md`.

## 5. Reduce to a minimal example

Strip the program down to the smallest possible Coal source that still triggers
the crash. The ideal minimal example is a single function with a single
expression.

## 6. Check the runtime library

If the kernel IR looks correct, the bug may be in `runtime-next/`. Check:
- Are GC allocations using `rt_alloc` / `rt_alloc_atomic` (never `malloc`)?
- Are fixed-width integer types used consistently?
- Is the runtime function signature in `RuntimeDefs.hs` consistent with the
  actual C implementation in `runtime-next/src/`?

## 7. Add debug instrumentation

Add `printf` calls to the C runtime at the crash site to trace values.
Recompile the runtime and re-link.

## 8. Only then propose a fix

Explain which invariant was violated and why the proposed change restores it.
Cite the legacy compiler's behaviour as ground truth.