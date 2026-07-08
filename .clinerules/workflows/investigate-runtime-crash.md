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

- If the kernel IR looks correct but the program still crashes, the bug is
  likely in the LLVM codegen or the runtime library.
- Compile with debug symbols and run under a debugger (`lldb` / `gdb`) to
  locate the crash site.

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

## 4. Reduce to a minimal example

Strip the program down to the smallest possible Coal source that still triggers
the crash. The ideal minimal example is a single function with a single
expression.

## 5. Check the runtime library

If the kernel IR looks correct, the bug may be in `runtime/`. Check:
- Are GC allocations using `rt_alloc` / `rt_alloc_atomic` (never `malloc`)?
- Are fixed-width integer types used consistently?
- Is the runtime function signature in `RuntimeDefs.hs` consistent with the
  actual C implementation in `runtime/src/`?

## 6. Add debug instrumentation

Add `printf` calls to the C runtime at the crash site to trace values.
Recompile the runtime and re-link.

## 7. Only then propose a fix

Explain which invariant was violated and why the proposed change restores it.